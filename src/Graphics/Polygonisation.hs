{-# language ScopedTypeVariables, TypeFamilies, FlexibleContexts, LambdaCase, AllowAmbiguousTypes #-}

module Graphics.Polygonisation
    ( createPolygonisationRenderer
    )
where

import Prelude hiding ((.), id, (<*))
import Control.Applicative (liftA2)
import Control.Category (Category((.)), id)
import Control.Exception (assert)
import Control.Lens ((&), (<&>), (^.), index)
import Control.Monad (forM_, replicateM, forM, foldM, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Exception
import Data.Bits
import Data.Bifunctor (second)
-- import qualified Deque.Strict as Deque
import Data.Int (Int8, Int32)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List ((\\), partition, delete, foldl', find, sortOn, sortBy, minimumBy, maximumBy, zipWith4, zipWith6)
import Data.Maybe
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word (Word8, Word16, Word32)
import GHC.Exts
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Codec.Picture as Juicy
import qualified Codec.Picture.Types as Juicy
import System.Log.Logger
import Text.Printf

import Common.Debug
import qualified Common.Random as Random

import Graphics.Color
import Graphics.CubeRoom
import Graphics.Geometry
import Graphics.Intersection
import Graphics.MarchingCube
import Graphics.Shaders
import Graphics.SkyBox
import Graphics.Texture

----------------------------------------------------------------------------------------------------------------------

{-
GPipe translation of:
https://developer.nvidia.com/gpugems/gpugems3/part-i-geometry/chapter-1-generating-complex-procedural-terrains-using-gpu
-}

----------------------------------------------------------------------------------------------------------------------

blockSize :: Num a => a
blockSize = 32

-- Should be even for upscale adapting purposes.
densityMargin :: Num a => a
densityMargin = 4

{- The margin is here to allow sampling outside the block when generating it and
the +1 is here because the density is sampled on the cube corners of a block.
-}
densityTextureSize :: Num a => a
densityTextureSize = blockSize + 1 + densityMargin * 2

-- Low frequency octates first, so a small count works for long-range sampling.
calculateDensity :: forall x. Int -> [Sampler3D (Format RFloat)] -> V3 (S x Float) -> V3 (S x Float) -> S x Float
calculateDensity octaveCount noiseSamplers offset p = density where
    sample i p a b = sample3D (cycle noiseSamplers !! i) (SampleLod 0) Nothing Nothing (p * a / 256) * b
    p' = p + offset
    base = p'^._z / 32
    -- base = (minB (norm p') (norm (p' + V3 40 0 0)) - 40) / 8
    -- base = (norm p' - 80) / 8
    -- base = (p'^._z + 1) / 16 + sin(p'^._x / 4 + p'^._y / 4) / 4
    density = base + sum (zipWith (\i (a, b) -> sample i p' a b) [0..octaveCount-1]
        [ (0.10, 6.40)
        , (0.20, 3.20)
        , (0.40, 1.60)
        , (0.80, 0.80)
        , (1.60, 0.40)
        , (3.20, 0.20)
        , (6.40, 0.10)
        ])

calculateDensity' :: forall x. Int -> [Sampler3D (Format RFloat)] -> V3 (S x Float) -> V3 (S x Float) -> S x Float
calculateDensity' _ _ offset p = density where
    p' = p + offset
    density = p'^._z / 16 + sin(p'^._x / 8) + cos(p'^._y / 12)

calculateDensity'' :: forall x. Int -> [Sampler3D (Format RFloat)] -> V3 (S x Float) -> V3 (S x Float) -> S x Float
calculateDensity'' _ _ offset p = density where
    p' = p + offset
    x = maxB 0 (minB 1 (p'^._x))
    y = maxB 0 (minB 1 (p'^._y))
    density =
        ifThenElse' (x ==* 0 &&* y ==* 0) 8 -- 6
        (ifThenElse' (x ==* 0 &&* y ==* 1) (-4) -- 4
        (ifThenElse' (x ==* 1 &&* y ==* 0) 9 -- 1
        (ifThenElse' (x ==* 1 &&* y ==* 1) (-3) -- (-5)
        0)))

{- Remember this function is only called for points on the cube border, not inside.

    upscale
  |<------------->|
    scale
  |<----->|

  [A]----(B)----[A]  Cases:
   |      |      |     [A] -> Even coordinates, nothing to do.
   |      |      |     (B) -> One odd coordinate, simple lerp.
  (B)----<C>----(B)    <C> -> Two odd coordinates, complex case.
   |      |      |
   |      |      |   Note:
  [A]----(B)----[A]    three odd coordinates are impossible on the cube border.
-}
getAdaptiveDensity
    :: Sampler1D (Format RInt)
    -> Sampler1D (Format RGBAInt)
    -> (V3 FFloat -> FFloat)
    -> V3 FFloat
    -> FFloat
getAdaptiveDensity aritySampler caseSampler getRawDensity (V3 x y z) =
    let
        isEven c = fract' (abs (c - densityMargin) / 2) <* 0.25

        -- densityTextureSize must be odd to have even coordinates at the borders
        split c = ifThenElse' (isEven c) (c, c) (c - 1, c + 1)
        (x1, x2) = split x
        (y1, y2) = split y
        (z1, z2) = split z

        mean n ds = sum ds / n

        getArity :: FInt -> FInt
        getArity = texelFetch1D aritySampler (pure 0) 0

        getCase :: FInt -> V4 FInt
        getCase = texelFetch1D caseSampler (pure 0) 0

        faceCaseFromDensities :: [FFloat] -> FInt
        faceCaseFromDensities densities = foldl1 or' (zipWith (\i d -> ifB (d >=* 0) (fromIntegral ((2^(3 - i))::Int)) 0) [0..] densities)

        pickFrom4 :: FInt -> [FFloat] -> FFloat -- GPipe eDSL definitely lacks arrays.
        pickFrom4 i values = let [v0, v1, v2, v3] = values in caseB i
            [ ((==* 0), v0)
            , ((==* 1), v1)
            , ((==* 2), v2)
            , ((==* 3), v3)
            ] 0

        notHappening = 0

        complexCase [d0, d1, d3, d2] = complexCase' [d0, d1, d2, d3]

        complexCase' :: [FFloat] -> FFloat
        complexCase' densities = v where
            faceCase = faceCaseFromDensities densities
            n = getArity faceCase
            V4 i0 i1 i2 i3 = getCase faceCase
            v = caseB n
                [ ((==* 0), handleCase0)
                , ((==* 2), handleCase2)
                , ((==* 4), handleCase4)
                ] notHappening

            handleCase0 = handleCaseX

            handleCase2 = (pickFrom4 i0 densities + pickFrom4 i1 densities) / 2

            handleCase4 = v where
                v11 = pickFrom4 i0 densities
                v12 = pickFrom4 i1 densities
                v21 = pickFrom4 i2 densities
                v22 = pickFrom4 i3 densities
                a = (v11 + v22) / 2
                b = v11 / (v11 - v12) + v22 / (v22 - v21)
                (a', b') = ifThenElse' (b <* 1)
                    (a, b)
                    ((v12 + v21) / 2, v12 / (v12 - v11) + v21 / (v21 - v22))
                v = ifThenElse' (abs b' <* epsilon)
                    ((v11 + v12 + v21 + v22) / 4)
                    (a' - a' / b')

            handleCaseX = v where
                v11 = pickFrom4 0 densities
                v12 = pickFrom4 1 densities
                v21 = pickFrom4 2 densities
                v22 = pickFrom4 3 densities
                v = (v11 + v12 + v21 + v22) / 4

    in  ifThenElse' (x1 /=* x2)
            (ifThenElse' (y1 /=* y2)
                (ifThenElse' (z1 /=* z2)
                    (mean 8 [ getRawDensity (V3 x y z) | x <- [x1, x2], y <- [y1, y2], z <- [z1, z2] ]) -- Should not happen on the cube border itself (but could happen inside the border)
                    (complexCase [ getRawDensity (V3 x y z) | x <- [x1, x2], y <- [y1, y2] ])
                )
                (ifThenElse' (z1 /=* z2)
                    (complexCase [ getRawDensity (V3 x y z) | z <- [z1, z2], x <- [x1, x2] ])
                    (mean 2 [ getRawDensity (V3 x y z) | x <- [x1, x2] ])
                )
            )
            (ifThenElse' (y1 /=* y2)
                (ifThenElse' (z1 /=* z2)
                    (complexCase [ getRawDensity (V3 x y z) | y <- [y1, y2], z <- [z1, z2] ])
                    (mean 2 [ getRawDensity (V3 x y z) | y <- [y1, y2] ])
                )
                (ifThenElse' (z1 /=* z2)
                     (mean 2 [ getRawDensity (V3 x y z) | z <- [z1, z2] ])
                    (getRawDensity (V3 x y z))
                )
            )

{-
   0             1
  [A]----(B)----[A]
   |      |      |
   |      |      |
  (B)----<C>----(B)
   |      |      |
   |      |      |
  [A]----(B)----[A]
   3             2
-}
--                         0      1      2      3
generateCaseUpscaleList [False, False, False, False] = []
generateCaseUpscaleList [False, False, False, True] = [0, 2]
generateCaseUpscaleList [False, False, True, False] = [1, 3]
generateCaseUpscaleList [False, False, True, True] = [1, 2, 3, 0]
generateCaseUpscaleList [False, True, False, False] = [0, 2]
generateCaseUpscaleList [False, True, False, True] = [] -- S
generateCaseUpscaleList [False, True, True, False] = [0, 1, 2, 3]
generateCaseUpscaleList [False, True, True, True] = [1, 3]
generateCaseUpscaleList [True, False, False, False] = [1, 3]
generateCaseUpscaleList [True, False, False, True] = [0, 1, 2, 3]
generateCaseUpscaleList [True, False, True, False] = [] -- S
generateCaseUpscaleList [True, False, True, True] = [0, 2]
generateCaseUpscaleList [True, True, False, False] = [1, 2, 3, 0]
generateCaseUpscaleList [True, True, False, True] = [1, 3]
generateCaseUpscaleList [True, True, True, False] = [2, 0]
generateCaseUpscaleList [True, True, True, True] = []
generateCaseUpscaleList _ = error "aboveness must contains data for 4 vertices"

{- Create a renderer to fill the provided 3D density texture by sampling the
density function at the given offset and scale. The provided noise textures
are used by the density function.

    densityTexture(p) = density(offset + (p + densityMargin) * scale)

-}
createFillDensityRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m)
    => Buffer os (Uniform (B3 Float, B Float)) -- ^ A UBO storing the offset and scale for the density texture to fill.
    -> Texture3D os (Format RFloat) -- ^ A 3x3 3D texture storing a boolean (as a float) indicating if the adjacent block in the corresponding direction has a greater scale (by 2 if any).
    -> Texture3D os (Format RFloat) -- ^ The 3D texture to fill by sampling the global density function (cf. calculateDensity).
    -> [Texture3D os (Format RFloat)] -- ^ An array of noise textures to feed to the density function.
    -> ContextT ctx os m (Render os ())
createFillDensityRenderer offsetAndScaleBuffer neighbourUpscaleTexture densityTexture noiseTextures = do
    planeBuffer :: Buffer os (B2 Float) <- newBuffer 4
    writeBuffer planeBuffer 0 [V2 (-1) (-1), V2 1 (-1), V2 (-1) 1, V2 1 1]

    heightBuffer :: Buffer os (B Float) <- newBuffer densityTextureSize
    writeBuffer heightBuffer 0 (fromIntegral <$> [0 .. densityTextureSize - 1])

    let caseUpscaleLists = map generateCaseUpscaleList (generateBoolCases 4)
        toPaddedCaseContent caseUpscaleList = let [i0, i1, i2, i3] = take 4 (caseUpscaleList ++ repeat 0) in fromIntegral <$> V4 i0 i1 i2 i3

    arityTexture :: Texture1D os (Format RInt) <- newTexture1D R8I 16 1
    writeTexture1D arityTexture 0 0 16 (map (fromIntegral . length) caseUpscaleLists :: [Int32])

    caseTexture :: Texture1D os (Format RGBAInt) <- newTexture1D RGBA8I 16 1
    writeTexture1D caseTexture 0 0 16 (map toPaddedCaseContent caseUpscaleLists :: [V4 Int8])

    shader :: CompiledShader os (Image (Format RFloat), PrimitiveArray Triangles (B Float, B2 Float))  <- compileShader $ do

        (offset, scale) <- getUniform (const (offsetAndScaleBuffer, 0))

        neighbourUpscaleSampler <- newSampler3D (const (neighbourUpscaleTexture, SamplerNearest, (pure ClampToEdge, undefined)))

        let filterMode = SamplerFilter Linear Linear Linear Nothing -- (Just 4)
            edgeMode = (pure Repeat, undefined)
        noiseSamplers <- forM noiseTextures $ \t -> newSampler3D (const (t, filterMode, edgeMode))

        ps :: primitiveStream Triangles ((VFloat, V2 VFloat), VInt) <- toPrimitiveStream snd
            <&> withInputIndices (\p indices -> (p, inputInstanceID indices))
        let ps' :: primitiveStream Triangles ((VPos, VInt), VFloat) = ps
                <&> (\((h, V2 x y), i) -> ((V4 x y 0 1, i), h))

        gs :: GeometryStream (Geometry Triangles ((VPos, VInt), VFloat)) <- geometrize ps'
        let makeTriangle :: Geometry Triangles ((VPos, VInt), VFloat) -> GGenerativeGeometry Triangles ((VPos, VInt), VFloat)
            makeTriangle (Triangle p1 p2 p3) = generativeTriangleStrip
                & emitVertexPositionAndLayer p1
                & emitVertexPositionAndLayer p2
                & emitVertexPositionAndLayer p3
                & endPrimitive
            gs' :: GeometryStream (GGenerativeGeometry Triangles ((VPos, VInt), VFloat)) = makeTriangle <$> gs

        let rasterOptions = const (Front, ViewPort 0 (pure densityTextureSize), DepthRange 0 1)
            maxVertices = 3
        fs <- generateAndRasterize rasterOptions maxVertices gs'
            <&> withRasterizedInfo (\h info -> let V4 x y _ _ = rasterizedFragCoord info in V3 x y h)

        aritySampler <- newSampler1D (const (arityTexture, SamplerNearest, (ClampToEdge, undefined)))
        caseSampler <- newSampler1D (const (caseTexture, SamplerNearest, (ClampToEdge, undefined)))

        -- Pixel centers are located at half-pixel centers and need to be shifted (z is fine).
        -- (cf. https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_FragCoord.xhtml)
        let fs' = getDensity' . (\p -> p - V3 0.5 0.5 0) <$> fs

            getDensity' :: V3 FFloat -> FFloat
            getDensity' p =
                let upscale = getUpscale p
                in  ifThenElse' (upscale >* 0.5)
                    (getAdaptiveDensity aritySampler caseSampler getDensity p)
                    (getDensity p)

            getDensity :: V3 FFloat -> FFloat
            getDensity = calculateDensity 7 noiseSamplers (offset - scale *^ pure densityMargin) . (scale *^)

            -- Return the max upscale when the sampling is done on an edge.
            getUpscale :: V3 FFloat -> ColorSample F RFloat
            getUpscale p =
                let dl = toDiscreteLocationVector p
                    getIt = maximumB . map (texelFetch3D neighbourUpscaleSampler (pure 0) 0 . (+ V3 1 1 1))
                    (a, b) = (-1, 1)
                in  caseB dl
                        [ ((==* V3 a a 0), getIt [dl, V3 a 0 0, V3 0 a 0])
                        , ((==* V3 a b 0), getIt [dl, V3 a 0 0, V3 0 b 0])
                        , ((==* V3 b a 0), getIt [dl, V3 b 0 0, V3 0 a 0])
                        , ((==* V3 b b 0), getIt [dl, V3 b 0 0, V3 0 b 0])
                        --
                        , ((==* V3 a 0 a), getIt [dl, V3 a 0 0, V3 0 0 a])
                        , ((==* V3 a 0 b), getIt [dl, V3 a 0 0, V3 0 0 b])
                        , ((==* V3 b 0 a), getIt [dl, V3 b 0 0, V3 0 0 a])
                        , ((==* V3 b 0 b), getIt [dl, V3 b 0 0, V3 0 0 b])
                        --
                        , ((==* V3 0 a a), getIt [dl, V3 0 a 0, V3 0 0 a])
                        , ((==* V3 0 a b), getIt [dl, V3 0 a 0, V3 0 0 b])
                        , ((==* V3 0 b a), getIt [dl, V3 0 b 0, V3 0 0 a])
                        , ((==* V3 0 b b), getIt [dl, V3 0 b 0, V3 0 0 b])
                        ] (getIt [dl])

            toDiscreteLocationVector :: V3 FFloat -> V3 FInt
            toDiscreteLocationVector p =
                let toUnitary c = guardedB undefined
                        [ (c <* densityMargin + 0.5, -1)
                        , (c <* densityTextureSize - densityMargin - 1.5, 0)
                        ] 1
                in  toUnitary <$> p

        draw (const NoBlending) fs' $
            drawColor (\env -> (fst env, True, False))

    assert (head (texture3DSizes densityTexture) == pure densityTextureSize) $ return $ do
        plane <- newVertexArray planeBuffer
        height <- newVertexArray heightBuffer
        let toV3 :: B2 Float -> B Float -> (B Float, B2 Float)
            toV3 i b = (b, i)
        let primitiveArray = toPrimitiveArrayInstanced TriangleStrip toV3 plane height
        image <- getLayeredTextureImage densityTexture 0
        shader (image, primitiveArray)

{- Construct the ordered list of all the numbers up to 2^depth in a binary form stored as boolean.
Per instance, with a deph of 3, the returned list will be:
    [[False, False, False]
    ,[False, False, True ]
    ,[False, True,  False]
    ,[False, True,  True ]
    ,[True,  False, False]
    ,[True,  False, True ]
    ,[True,  True,  False]
    ,[True,  True,  True ]
    ]
-}
generateBoolCases :: Int -> [[Bool]]
generateBoolCases 0 = [[]]
generateBoolCases i = [ x:xs | x <- [False, True], xs <- generateBoolCases (i - 1) ]

-- 2^(7-i) benefits of being statically evaluated.
cellCaseFromDensities :: [VFloat] -> VInt
cellCaseFromDensities densities= foldl1 or' (zipWith (\i d -> ifB (d >* 0) (fromIntegral ((2^(7-i))::Int)) 0) [0..] densities) -- TODO (length densities - 1 - i)

instance Bits' Int where
    and' = (.&.)
    or' = (.|.)
    xor' = xor
    complement' = complement
    shiftL' = shiftL
    shiftR' = shiftR
    bitSize' = finiteBitSize

encode :: (Num a, Bits' a) => [Int] -> [a] -> a
encode sizes values = compositeValue where
    positions = map fromIntegral $ scanl (+) 0 (init sizes)
    compositeValue = foldl1 or' (zipWith shiftL' values positions)

decode :: (Num a, Bits' a) => [Int] -> a -> [a]
decode sizes compositeValue = zipWith extract positions sizes where
    positions = map fromIntegral $ scanl (+) 0 (init sizes)
    extract position size = and' (shiftR' compositeValue position) (2^size-1)

decodeInHaskell :: (Num a, Bits a) => [Int] -> a -> [a]
decodeInHaskell sizes compositeValue = zipWith extract positions sizes where
    positions = map fromIntegral $ scanl (+) 0 (init sizes)
    extract position size = (.&.) (shiftR compositeValue position) (2^size-1)

isRayWithCubeIntersect :: (V3 VFloat, V3 VFloat) -> (VFloat, V3 VFloat) -> VBool
isRayWithCubeIntersect (orig, dir) (scale, lowerCorner) =
    let minBox = lowerCorner
        maxBox = lowerCorner + pure scale
    in  boundingBoxHit (minBox, maxBox) (orig, dir)

boundingBoxHit :: (V3 VFloat, V3 VFloat) -> (V3 VFloat, V3 VFloat) -> VBool
boundingBoxHit box ray = hit where
    dims = [0, 1, 2] :: [VInt]

    toList (V3 x y z) = [x, y, z]
    fromList [x, y, z] = V3 x y z
    fromList _ = undefined

    minBox = toList (fst box)
    maxBox = toList (snd box)
    origin = toList (fst ray)
    direction = toList (snd ray)

    eLocRight = 0
    eLocLeft = 1
    eLocMiddle = 2

    -- Find candidate planes; this loop can be avoided if rays cast all from the eye (assume perpsective view)
    findCandidatePlane :: VFloat -> VFloat -> VFloat -> (VInt, VFloat, S V Bool)
    findCandidatePlane o min max = guardedB undefined
        [ (o <* min, (eLocLeft, min, false))
        , (o >* max, (eLocRight, max, false))
        ] (eLocMiddle, -1, true)

    (quadrant, candidatePlane, inside) = unzip3 $ zipWith3 findCandidatePlane origin minBox maxBox

    -- Ray origin inside bounding box
    hit = ifThenElse' (andB inside)
        true
        coord where
            -- Calculate T distances to candidate planes
            distanceToCandidatePlane q cp o d = ifThenElse' (q /=* eLocMiddle &&* d /=* 0)
                ((cp - o) / d)
                (-1)
            indexedMaxT = zip dims (zipWith4 distanceToCandidatePlane quadrant candidatePlane origin direction)

            -- Get largest of the maxT's for final choice of intersection
            (whichPlane, maxT_whichPlane) = maximumByB (comparingB snd) indexedMaxT

            -- Check final candidate actually inside box
            filterCandidate i o d min max cp =
                let c = o + maxT_whichPlane * d;
                in  ifThenElse' (whichPlane /=* i)
                        (min <=* c &&* c <=* max)
                        true

            coord = ifThenElse' (maxT_whichPlane <* 0)
                false
                (andB $ zipWith6 filterCandidate dims origin direction minBox maxBox candidatePlane)

createGenerateIntersectionRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m)
    => Window os RGBAFloat Depth
    -> Buffer os (Uniform (B3 Float, B Float))
    -> Buffer os (Uniform (B3 Float, B3 Float))
    -> Texture3D os (Format RFloat)
    -> [Texture3D os (Format RFloat)]
    -> ContextT ctx os m (Buffer os (B Word32, V2 (B4 Float)) -> Render os ())
createGenerateIntersectionRenderer window offsetAndScaleBuffer rayBuffer densityTexture noiseTextures = do
    -- We consider the cells in the block.
    let cellCount = blockSize ^ 3

    -- All the 'cellCount' cell coordinates in the block.
    cellPositionBuffer :: Buffer os (B3 Int8) <- newBuffer cellCount
    writeBuffer cellPositionBuffer 0 [ fromIntegral <$> V3 x y z | x <- [0 .. blockSize - 1], y <- [0 .. blockSize - 1], z <- [0 .. blockSize - 1] ]

    -- All positions of the not empty intersecting cubes combined with their interface cell cases [1-254], packing them as 'z8_y8_x8_case8'.

    listIntersectingCells :: CompiledShader os (PrimitiveArray Points (B3 Int8), Buffer os (B Word32, V2 (B4 Float)))  <- compileShader $ do

        densitySampler <- newSampler3D (const (densityTexture, SamplerNearest, (pure ClampToEdge, undefined)))

        ps :: primitiveStream Points (V3 VInt) <- toPrimitiveStream fst

        gs :: GeometryStream (Geometry Points (V3 VInt)) <- geometrize ps

        (offset, scale) <- getUniform (const (offsetAndScaleBuffer, 0))
        (orig, dir) <- getUniform (const (rayBuffer, 0))

        let makeMarks :: Geometry Points (V3 VInt) -> GGenerativeGeometry Points (VWord, V2 (V4 VFloat))
            makeMarks (Point p) = marks
                where
                    getDensity :: V3 VInt -> VFloat
                    getDensity = texelFetch3D densitySampler (pure 0) 0 . (+ pure densityMargin)

                    densities :: [VFloat]
                    densities = map (getDensity . (p +)) cube

                    cellCase = cellCaseFromDensities densities

                    intersectWithRay :: VBool
                    intersectWithRay =
                        let lowerCorner = offset + (toFloat <$> p) ^* scale
                        in  isRayWithCubeIntersect (orig, dir) (scale, lowerCorner)

                    marks :: GGenerativeGeometry Points (VWord, V2 (V4 VFloat))
                    marks = ifThenElse (cellCase /=* 0 &&* cellCase /=* 255 &&* intersectWithRay) emitMark id generativePoints

                    emitMark :: GGenerativeGeometry Points (VWord, V2 (V4 VFloat)) -> GGenerativeGeometry Points (VWord, V2 (V4 VFloat))
                    emitMark =
                        let V3 x y z = toWord <$> p
                            c = toWord cellCase
                            z8_y8_x8_case8 = encode [8, 8, 8, 8] [z, y, x, c]
                            [d0, d1, d2, d3, d4, d5, d6, d7] = densities
                        in  endPrimitive . emitVertex (z8_y8_x8_case8, V2 (V4 d0 d1 d2 d3) (V4 d4 d5 d6 d7))

            gs' :: GeometryStream (GGenerativeGeometry Points (VWord, V2 (V4 VFloat))) = makeMarks <$> gs

        let maxVertices = 1
        drawNothing window snd Queried maxVertices gs'

    return $ \intersectionBuffer -> do
        positions <- toPrimitiveArray PointList <$> newVertexArray cellPositionBuffer
        listIntersectingCells (positions, intersectionBuffer)

{- Create a renderer to generate the block at the given offset and scale. The provided noise textures
are used by the density function.

    densityTexture(p) = density(offset + (p + densityMargin) * scale)

-}
createGenerateBlockRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m)
    => Window os RGBAFloat Depth
    -> Buffer os (Uniform (B3 Float, B Float))
    -> Texture3D os (Format RFloat)
    -> [Texture3D os (Format RFloat)]
    -> ContextT ctx os m (Buffer os (B4 Float, B3 Float) -> Buffer os (B Word32) -> Render os ())
createGenerateBlockRenderer window offsetAndScaleBuffer densityTexture noiseTextures = do
    -- We consider the cells in the block plus those on the "highest" border (see edgeBuffer below).
    let extCellCount = (blockSize + 1) ^ 3

    -- All the 'extCellCount' cell coordinates in the generated block.
    cellPositionBuffer :: Buffer os (B3 Int8) <- newBuffer extCellCount
    writeBuffer cellPositionBuffer 0 [ fromIntegral <$> V3 x y z | x <- [0 .. blockSize], y <- [0 .. blockSize], z <- [0 .. blockSize] ]

    -- All positions combined with their interface cell cases [1-254], packing them as 'z8_y8_x8_case8'.
    -- Position with empty (0) or full (255) cases are discarded.
    interfaceCellBuffers :: Buffer os (B Word32) <- newBuffer extCellCount

    listInterfaceCells :: CompiledShader os (PrimitiveArray Points (B3 Int8))  <- compileShader $ do

        densitySampler <- newSampler3D (const (densityTexture, SamplerNearest, (pure ClampToEdge, undefined)))

        ps :: primitiveStream Points (V3 VInt) <- toPrimitiveStream id

        gs :: GeometryStream (Geometry Points (V3 VInt)) <- geometrize ps

        let makeMarks :: Geometry Points (V3 VInt) -> GGenerativeGeometry Points VWord
            makeMarks (Point p) = marks
                where
                    getDensity :: V3 VInt -> VFloat
                    getDensity = texelFetch3D densitySampler (pure 0) 0 . (+ pure densityMargin)

                    densities :: [VFloat]
                    densities = map (getDensity . (p +)) cube

                    cellCase = cellCaseFromDensities densities

                    marks :: GGenerativeGeometry Points VWord
                    marks = ifThenElse (cellCase /=* 0 &&* cellCase /=* 255) emitMark id generativePoints

                    emitMark :: GGenerativeGeometry Points VWord -> GGenerativeGeometry Points VWord
                    emitMark =
                        let V3 x y z = toWord <$> p
                            c = toWord cellCase
                            z8_y8_x8_case8 = encode [8, 8, 8, 8] [z, y, x, c]
                        in  endPrimitive . emitVertex z8_y8_x8_case8

            gs' :: GeometryStream (GGenerativeGeometry Points VWord) = makeMarks <$> gs

        let maxVertices = 1
        drawNothing window (const interfaceCellBuffers) Stored maxVertices gs'

    -- Translatation of all (position, case) into 0-3 edges packed as z8_y8_x8_edge8.
    -- An edge combines the original coordinates with an index for the edge ([(0, 1), (1, 2), (2, 4)]).
    -- Only the separationg edges (ie where an interface triangle has a vertice) are kept, the others are ignored.
    -- Since all edges are shared by two cells, we only need to store half of them (those in the lowest side).
    -- That’s also the reason why we process the cells on the highest border: to have enough information for the cells inside.
    edgeBuffer :: Buffer os (B Word32) <- newBuffer (extCellCount * 3)

    listVerticesToGenerate :: CompiledShader os (Buffer os (B Word32), PrimitiveArray Points (B Word32))  <- compileShader $ do

        ps :: primitiveStream Points VWord <- toFeedbackPrimitiveStream fst snd

        gs :: GeometryStream (Geometry Points VWord) <- geometrize ps

        let makeProtoVertices :: Geometry Points VWord -> GGenerativeGeometry Points VWord
            makeProtoVertices (Point z8_y8_x8_case8) = protoVertices
                where
                    [z, y, x, c] = decode [8, 8, 8, 8] z8_y8_x8_case8
                    p = V3 x y z

                    protoVertices :: GGenerativeGeometry Points VWord
                    protoVertices = foldl
                        (\gg (edgeIndice, verticeIndice) -> ifThenElse (hasAnEdge verticeIndice) (emitProtoVertice verticeIndice) id gg) generativePoints
                        [(0, 1), (1, 2), (2, 4)]

                    hasAnEdge :: VWord -> VBool
                    hasAnEdge verticeIndice = and' (shiftR' c 7) 1 /=* and' (shiftR' c (7-verticeIndice)) 1

                    emitProtoVertice :: VWord -> GGenerativeGeometry Points VWord -> GGenerativeGeometry Points VWord
                    emitProtoVertice verticeIndice =
                        let z8_y8_x8_edge8 = encode [8, 8, 8, 8] [z, y, x, verticeIndice]
                        in  endPrimitive . emitVertex z8_y8_x8_edge8

            gs' :: GeometryStream (GGenerativeGeometry Points VWord) = makeProtoVertices <$> gs

        let maxVertices = 3
        drawNothing window (const edgeBuffer) Stored maxVertices gs'

    --  we rely on a texture to easily implement a constant array iteration in GPipe.
    cubeVerticeTexture :: Texture1D os (Format RGBInt) <- newTexture1D RGB8I (length cube) 1
    writeTexture1D cubeVerticeTexture 0 0 (length cube) (cube :: [V3 Int8])

    -- To calculate the occlusion using local ray tracing.
    let poissonOnSphere = map (\(x, y, z) -> V3 x y z) Random.poissonOnSphere
    poissonOnSphereTexture :: Texture1D os (Format RGBFloat) <- newTexture1D RGB16F (length poissonOnSphere) 1
    writeTexture1D poissonOnSphereTexture 0 0 (length poissonOnSphere) poissonOnSphere

    -- Translate the (position, edge) into vertices at the right position on each interface edge.
    -- Each vertice stores the position, the normal and the occlusion.
    -- The later is stored in the position w coordinate to save some space.
    generateVertices :: CompiledShader os (Buffer os (B4 Float, B3 Float), (Buffer os (B Word32), PrimitiveArray Points (B Word32)))  <- compileShader $ do

        (offset, scale) <- getUniform (const (offsetAndScaleBuffer, 0))

        raySampler <- newSampler1D (const (poissonOnSphereTexture, SamplerNearest, (ClampToEdge, undefined)))

        let filterMode = SamplerFilter Linear Linear Linear Nothing -- (Just 4)
            edgeMode = (pure Repeat, undefined)
        noiseSamplers <- forM noiseTextures $ \t -> newSampler3D (const (t, filterMode, edgeMode))

        densitySampler <- newSampler3D (const (densityTexture, filterMode, (pure ClampToEdge, undefined)))

        cubeVerticeSampler <- newSampler1D (const (cubeVerticeTexture, SamplerNearest, (ClampToEdge, undefined)))

        ps :: primitiveStream Points VWord <- toFeedbackPrimitiveStream (fst . snd) (snd . snd)

        let toVertice :: VWord -> (V4 VFloat, V3 VFloat)
            toVertice z8_y8_x8_edge8 = vertice
                where
                    [z, y, x, verticeIndice] = map toInt $ decode [8, 8, 8, 8] z8_y8_x8_edge8
                    p = V3 x y z

                    getFloatDensity :: V3 VFloat -> VFloat
                    getFloatDensity v =
                        let uv = (v + pure densityMargin + pure 1) / densityTextureSize
                        in  sample3D densitySampler (SampleLod 0) Nothing Nothing uv

                    -- Long range implied.
                    calculateFloatDensity :: V3 VFloat -> VFloat
                    calculateFloatDensity v = calculateDensity 6 noiseSamplers offset (v ^* scale)

                    getDensity :: V3 VInt -> VFloat
                    getDensity = texelFetch3D densitySampler (pure 0) 0 . (+ pure densityMargin)

                    getCubeVertice :: VInt -> V3 VInt
                    getCubeVertice = texelFetch1D cubeVerticeSampler (pure 0) 0

                    vertice :: (V4 VFloat, V3 VFloat)
                    vertice = createVertice verticeIndice

                    getNormal :: V3 VFloat -> V3 VFloat
                    getNormal v = normal where
                        sample dv = getFloatDensity (v + dv / blockSize)
                        grad = V3
                            (sample (V3 1 0 0) - sample (V3 (-1) 0 0))
                            (sample (V3 0 1 0) - sample (V3 0 (-1) 0))
                            (sample (V3 0 0 1) - sample (V3 0 0 (-1)))
                        normal = signorm grad

                    getRayDir :: VInt -> V3 VFloat
                    getRayDir = texelFetch1D raySampler (pure 0) 0

                    calculateOcclusion :: V3 VFloat -> VFloat
                    calculateOcclusion v =
                        let rayCount = fromIntegral (length poissonOnSphere)
                            visibility = forN (0, rayCount) 0 $ \ray visibility ->
                                let dir = getRayDir ray
                                    shortRangeRayVisibility = forN (1, 4) 1 $ \step rayVisibility ->
                                        rayVisibility * saturate (getFloatDensity (v + dir ^* toFloat step) * 16 / scale) -- 8
                                    longRangeRayVisibility = forN (1, 4) 1 $ \step rayVisibility ->
                                        rayVisibility * saturate (calculateFloatDensity (v + dir ^* (32 * toFloat step)) * 2) -- 0.5
                                in  visibility + shortRangeRayVisibility * longRangeRayVisibility
                        in  1 - visibility / toFloat rayCount

                    createVertice i =
                        let v1 = p + getCubeVertice 0
                            v2 = p + getCubeVertice i
                            calculatePoint :: V3 VInt -> V3 VInt -> V3 VFloat
                            calculatePoint v1 v2 = (1 - a) *^ (toFloat <$> v1) + a *^ (toFloat <$> v2) where
                                (d1, d2) = (getDensity v1, getDensity v2)
                                a = d1 / (d1 - d2)
                            v = calculatePoint v1 v2
                            o = calculateOcclusion v
                            n = getNormal v

                            v3plus1 (V3 x y z) w = V4 x y z w
                        in  (v3plus1 (offset + v ^* scale) o, n)

        let ps' :: primitiveStream Points (V4 VFloat, V3 VFloat) = toVertice <$> ps

        gs :: GeometryStream (Geometry Points (V4 VFloat, V3 VFloat)) <- geometrize ps'

        let makeVertice :: Geometry Points (V4 VFloat, V3 VFloat) -> GGenerativeGeometry Points (V4 VFloat, V3 VFloat)
            makeVertice (Point (p, n)) = generativePoints
                & emitVertex (p, n)
                & endPrimitive

            gs' :: GeometryStream (GGenerativeGeometry Points (V4 VFloat, V3 VFloat)) = makeVertice <$> gs

        let maxVertices = 1
        drawNothing window fst Stored maxVertices gs'

    let size = blockSize + 1
    splatVertexIdsTexture :: Texture3D os (Format RWord) <- newTexture3D R32UI (V3 (3 * size) size size) 1

    splatVertexIndexes :: CompiledShader os (Image (Format RWord), (Buffer os (B Word32), PrimitiveArray Points (B Word32)))  <- compileShader $ do

        let extractVertice :: VWord -> V3 VInt
            extractVertice z8_y8_x8_edge8 = V3 x' y z
                where
                    [z, y, x, verticeIndice] = map toInt $ decode [8, 8, 8, 8] z8_y8_x8_edge8
                    x' = 3 * x + div' verticeIndice 2
        ps :: PrimitiveStream Points (V3 VInt, VWord) <- toFeedbackPrimitiveStream (fst . snd) (snd . snd) -- toPrimitiveStream (snd . snd)
            <&> withInputIndices (\p indices -> (extractVertice p, toWord $ inputVertexID indices))

        gs :: GeometryStream (Geometry Points (V3 VInt, VWord)) <- geometrize ps
        let
            toUV c m = (toFloat c + 0.5) / fromIntegral m * 2 - 1
            splat :: Geometry Points (V3 VInt, VWord) -> GGenerativeGeometry Points ((VPos, VInt), VWord)
            splat (Point (V3 x' y z, i)) = generativePoints
                & emitVertexPositionAndLayer ((V4 (toUV x' (3 * size)) (toUV y size) 0 1, z), i)
                & endPrimitive
            gs' :: GeometryStream (GGenerativeGeometry Points ((VPos, VInt), VWord)) = splat <$> gs

        let rasterOptions = const (Front, ViewPort 0 (V2 (3 * size) size), DepthRange 0 1)
            maxVertices = 1
        fs :: FragmentStream FWord <- generateAndRasterize rasterOptions maxVertices gs'

        draw (const NoBlending) fs $
            drawColor (\env -> (fst env, True, False))

    let protoTriangleLists = map generateCaseProtoTriangleList (generateBoolCases 8)
        toCaseContent :: [(EdgeIndice, EdgeIndice, EdgeIndice)] -> [V3 Int8]
        toCaseContent protoTriangleList = map (\(e1, e2, e3) -> fromIntegral <$> V3 e1 e2 e3) protoTriangleList
        toPaddedCaseContent protoTriangleList = take maxCellTriangleCount (toCaseContent protoTriangleList ++ repeat (V3 0 0 0))

    arityTexture :: Texture1D os (Format RInt) <- newTexture1D R8I 256 1
    writeTexture1D arityTexture 0 0 256 (map (fromIntegral . length) protoTriangleLists :: [Int32])

    caseTexture :: Texture2D os (Format RGBInt) <- newTexture2D RGB8I (V2 maxCellTriangleCount 256) 1
    writeTexture2D caseTexture 0 0 (V2 maxCellTriangleCount 256) (concatMap toPaddedCaseContent protoTriangleLists)

    let edgeToSplatCoords :: [(V3 Int, Int)]
        edgeToSplatCoords =
            [ (V3 0 0 0, 0)
            , (V3 0 0 0, 1)
            , (V3 0 0 0, 2)
            , (V3 0 0 0, 0)
            , (V3 1 0 0, 1)
            , (V3 1 0 0, 2)
            , (V3 0 0 0, 1)
            , (V3 0 1 0, 0)
            , (V3 0 1 0, 2)
            , (V3 1 0 0, 1)
            , (V3 0 1 0, 0)
            , (V3 1 1 0, 2)
            , (V3 0 0 0, 2)
            , (V3 0 0 1, 0)
            , (V3 0 0 1, 1)
            , (V3 1 0 0, 2)
            , (V3 0 0 1, 0)
            , (V3 1 0 1, 1)
            , (V3 0 1 0, 2)
            , (V3 0 0 1, 1)
            , (V3 0 1 1, 0)
            , (V3 1 1 0, 2)
            , (V3 1 0 1, 1)
            , (V3 0 1 1, 0)
            ]

    edgeToSplatCoordsTexture :: Texture1D os (Format RWord) <- newTexture1D R8UI (length edgeToSplatCoords) 1
    writeTexture1D edgeToSplatCoordsTexture 0 0 (length edgeToSplatCoords) (map (\(V3 dx dy dz, o) -> fromIntegral $ encode [2, 2, 2, 2] [dx, dy, dz, o] :: Word32) edgeToSplatCoords)

    generateIndices :: CompiledShader os (Buffer os (B Word32), (Buffer os (B Word32), PrimitiveArray Points (B Word32)))  <- compileShader $ do

        aritySampler <- newSampler1D (const (arityTexture, SamplerNearest, (ClampToEdge, undefined)))
        caseSampler <- newSampler2D (const (caseTexture, SamplerNearest, (pure ClampToEdge, undefined)))
        edgeToSplatCoordsSampler <- newSampler1D (const (edgeToSplatCoordsTexture, SamplerNearest, (ClampToEdge, undefined)))
        splatVertexIdsSampler <- newSampler3D (const (splatVertexIdsTexture, SamplerNearest, (pure ClampToEdge, undefined)))

        -- FIXME The VS forces us to a 4 bytes aligned type which ends up being the type
        -- used for the index buffer filled by the TF (BPacked Word16 would suffice).
        ps :: primitiveStream Points VWord <- toFeedbackPrimitiveStream (fst . snd) (snd . snd)

        gs :: GeometryStream (Geometry Points VWord) <- geometrize ps

        let makeTriangles :: Geometry Points VWord -> GGenerativeGeometry Triangles VWord
            makeTriangles (Point z8_y8_x8_case8) = triangles'
                where
                    [z, y, x, c] = decode [8, 8, 8, 8] z8_y8_x8_case8
                    cellCase = toInt c

                    getArity :: VInt -> VInt
                    getArity = texelFetch1D aritySampler (pure 0) 0

                    getCase :: V2 VInt -> V3 VInt
                    getCase = texelFetch2D caseSampler (pure 0) 0

                    getEdgeToSplatCoords :: VInt -> VWord
                    getEdgeToSplatCoords = texelFetch1D edgeToSplatCoordsSampler (pure 0) 0

                    getIndex :: VInt -> VWord
                    getIndex e = texelFetch3D splatVertexIdsSampler (pure 0) 0 p
                        where
                            p = toInt <$> V3 (3 * (x + dx) + o) (y + dy) (z + dz)
                            [dx, dy, dz, o] = decode [2, 2, 2, 2] (getEdgeToSplatCoords e)

                    count = getArity cellCase

                    triangles' = ifThenElse'
                        (x <* blockSize &&* y <* blockSize &&* z <* blockSize)
                        triangles
                        generativeTriangleStrip

                    triangles :: GGenerativeGeometry Triangles VWord
                    triangles = forN (0, count) generativeTriangleStrip emitTriangleIndexes

                    emitTriangleIndexes i =
                        let V3 e1 e2 e3 = getCase (V2 i cellCase)
                            i1 = getIndex e1
                            i2 = getIndex e2
                            i3 = getIndex e3
                        in  endPrimitive .
                            emitVertex i1 .
                            emitVertex i3 .
                            emitVertex i2

            gs' :: GeometryStream (GGenerativeGeometry Triangles VWord) = gs <&> makeTriangles

        let maxVertices = maxCellTriangleCount * 3
        drawNothing window fst Queried maxVertices gs'

    return $ \blockBuffer indexBuffer -> do
        positions <- toPrimitiveArray PointList <$> newVertexArray cellPositionBuffer
        listInterfaceCells positions
        protoBlocks1 <- toPrimitiveArray PointList <$> newVertexArray interfaceCellBuffers
        listVerticesToGenerate (interfaceCellBuffers, protoBlocks1)
        protoBlocks2 <- toPrimitiveArray PointList <$> newVertexArray edgeBuffer
        generateVertices (blockBuffer, (edgeBuffer, protoBlocks2))
        splatVertexIds <- getLayeredTextureImage splatVertexIdsTexture 0
        splatVertexIndexes (splatVertexIds, (edgeBuffer, protoBlocks2))
        generateIndices (indexBuffer, (interfaceCellBuffers, protoBlocks1))

createIndexedBlockRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m)
    => Window os RGBAFloat Depth
    -> Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float))
    -> Buffer os (Uniform FogB)
    -> Buffer os (Uniform DirectionLightB)
    -> ContextT ctx os m ((ViewPort, (Buffer os (B4 Float, B3 Float), Buffer os (B Word32))) -> Render os ())
createIndexedBlockRenderer window projectionBuffer fogBuffer sunBuffer = do

    noiseTexture :: Texture2D os (Format RFloat) <- generate2DNoiseTexture (16, 16)

    {-
    Just altMap <- loadImage "data/altmap.tga"
    generateTexture2DMipmap altMap
    -}

    {-
    Just mosaicDiffuseTexture <- loadImage "data/TILEABLE_RED_MOSAIC_TEXTURE.jpg"
    generateTexture2DMipmap mosaicDiffuseTexture

    Just mosaicNormalTexture <- loadImage "data/TILEABLE_RED_MOSAIC_TEXTURE_NORMAL.jpg"
    generateTexture2DMipmap mosaicNormalTexture

    Just mosaicSpecularTexture <- loadImage "data/TILEABLE_RED_MOSAIC_TEXTURE_SPECULAR.jpg"
    generateTexture2DMipmap mosaicSpecularTexture
    -}

    Just groundDiffuseTexture <- loadImage "data/Seamless_cracked_sand_ground_texture.jpg"
    generateTexture2DMipmap groundDiffuseTexture

    Just groundNormalTexture <- loadImage "data/Seamless_cracked_sand_ground_texture_NORMAL.jpg"
    generateTexture2DMipmap groundNormalTexture

    Just grassDiffuseTexture <- loadImage "data/seamless_green_grass_rough_DIFFUSE.jpg"
    generateTexture2DMipmap grassDiffuseTexture

    Just grassNormalTexture <- loadImage "data/seamless_green_grass_rough_NORMAL.jpg"
    generateTexture2DMipmap grassNormalTexture

    shader :: CompiledShader os (ViewPort, (Buffer os (B Word32), PrimitiveArray Triangles (B4 Float, B3 Float)))  <- compileShader $ do

        (projectionMat, cameraMat, cameraPos) <- getUniform (const (projectionBuffer, 0))
        let modelViewProj = projectionMat !*! cameraMat

        ps :: primitiveStream Triangles (V4 VFloat, V3 VFloat) <- toFeedbackPrimitiveStream (fst . snd) (snd . snd)
        let ps' :: primitiveStream Triangles (VPos, (V4 VFloat, V3 VFloat, V3 VFloat)) = ps <&> \(po, n) -> (modelViewProj !* point (po ^. _xyz), (po, n, cameraPos))

        fog :: FogS F <- getUniform (const (fogBuffer, 0))
        sun :: DirectionLightS F <- getUniform (const (sunBuffer, 0))

        noiseSampler <- newSampler2D (const (noiseTexture, SamplerFilter Linear Linear Linear (Just 4), (pure Mirror, undefined)))
        {-
        altMapSampler <- newSampler2D (const (altMap, SamplerFilter Linear Linear Linear (Just 4), (pure Mirror, undefined)))
        -}

        {-
        mosaicDiffuseSampler <- newSampler2D (const (mosaicDiffuseTexture, SamplerFilter Linear Linear Linear (Just 4), (pure Repeat, undefined)))
        mosaicNormalSampler <- newSampler2D (const (mosaicNormalTexture, SamplerFilter Linear Linear Linear (Just 4), (pure Repeat, undefined)))
        -}

        groundDiffuseSampler <- newSampler2D (const (groundDiffuseTexture, SamplerFilter Linear Linear Linear (Just 4), (pure Repeat, undefined)))
        groundNormalSampler <- newSampler2D (const (groundNormalTexture, SamplerFilter Linear Linear Linear (Just 4), (pure Repeat, undefined)))

        grassDiffuseSampler <- newSampler2D (const (grassDiffuseTexture, SamplerFilter Linear Linear Linear (Just 4), (pure Repeat, undefined)))
        grassNormalSampler <- newSampler2D (const (grassNormalTexture, SamplerFilter Linear Linear Linear (Just 4), (pure Repeat, undefined)))

        let rasterOptions = \(viewPort, _) -> (Front, viewPort, DepthRange 0 1)
        fs :: FragmentStream (V4 FFloat, FragDepth) <- rasterize rasterOptions ps' <&> withRasterizedInfo (\(po, n, cp) ri ->
                let lightingContext =
                        ( cp
                        , fog
                        , sun
                        , 0.02 -- material specular intensity
                        , 8 -- material specular power
                        )
                    p = po ^. _xyz
                    o = po ^. _w

                    {-
                    sample p = sample2D noiseSampler (SampleLod 0) Nothing Nothing (p / 1000)
                    uv = V2 (sample $ p^. _xy) (p^. _z / 64 + 32)
                    diffuse = sample2D altMapSampler SampleAuto Nothing Nothing uv
                    -}

                    samplingScale = blockSize * 2
                    squareDot c n = let d = abs (c `dot` n) in d * d

                    triplanarSample =
                        let sampleDirection s f c = sample2D s SampleAuto Nothing Nothing (f p / samplingScale) ^* (c `squareDot` n)
                            dz = sampleDirection grassDiffuseSampler (^. _xy) (V3 0 0 1)
                            dy = sampleDirection groundDiffuseSampler (^. _zx) (V3 0 1 0)
                            dx = sampleDirection groundDiffuseSampler (^. _yz) (V3 1 0 0)
                        in  (dz + dy + dx)
                    diffuse = triplanarSample

                    triplanarNormalSample =
                        let sampleDirection s f c =
                                let d = c `dot` n
                                    r = ifThenElse' (d <* 0) (V3 1 1 (-1)) (V3 1 1 1)
                                    n' = ((\v -> v * 2 - 1) <$> sample2D s SampleAuto Nothing Nothing (f p / samplingScale)) ^* (d * d)
                                in  r * n'
                            nz = sampleDirection grassNormalSampler (^. _xy) (V3 0 0 1) ^. _xyz
                            ny = sampleDirection groundNormalSampler (^. _zx) (V3 0 1 0) ^. _yzx
                            nx = sampleDirection groundNormalSampler (^. _yz) (V3 1 0 0) ^. _zxy
                        in  signorm (nz + ny + nx)
                    n' = triplanarNormalSample

                    m = point $ diffuse ^* saturate (1 - o)
                    c = getSunlight undefined n' Nothing p m 1 lightingContext
                in  (c, rasterizedFragCoord ri ^. _z))

        let colorOption = ContextColorOption NoBlending (pure True)
            depthOption = DepthOption Less True
        drawWindowColorDepth (const (window, colorOption, depthOption)) fs

    return $ \(viewPort, (blockBuffer, indexBuffer)) -> do
        indexedBlock <- toPrimitiveArrayIndexed TriangleList
            <$> newIndexArray indexBuffer Nothing
            <*> newVertexArray blockBuffer
        shader (viewPort, (indexBuffer, indexedBlock))

createBlockOutlinesRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m)
    => Window os RGBAFloat Depth
    -> Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float))
    -> Buffer os (B Float, B3 Float)
    -> ContextT ctx os m (ViewPort -> Int -> Render os ())
createBlockOutlinesRenderer window projectionBuffer outlineBuffer = do

    blockOutlineBuffer :: Buffer os (B3 Float) <- newBuffer (length cube)
    writeBuffer blockOutlineBuffer 0 cube

    let edges = concatMap (\(i, j) -> [i, j]) $ filter (\(i, j) -> j > i) cubeEdges
    blockOutlineIndexBuffer :: Buffer os (BPacked Word8) <- newBuffer (length edges)
    writeBuffer blockOutlineIndexBuffer 0 (map fromIntegral edges)

    shader :: CompiledShader os (ViewPort, PrimitiveArray Lines (B3 Float, (B Float, B3 Float)))  <- compileShader $ do
        (projectionMat, cameraMat, _) <- getUniform (const (projectionBuffer, 0))
        let modelViewProj = projectionMat !*! cameraMat

        lines :: PrimitiveStream Lines (V3 VFloat, (VFloat, V3 VFloat)) <- toPrimitiveStream snd
        let
            projectedLines :: PrimitiveStream Lines (V4 VFloat, ())
            projectedLines =
                (\p -> (modelViewProj !* p, ())) .
                (\(p, (s, o)) -> point (s *^ p + o)) <$>
                lines

        let rasterOptions = \(viewPort, _) -> (Front, viewPort, DepthRange 0 1)
        fs :: FragmentStream (V4 FFloat, FragDepth) <- rasterize rasterOptions projectedLines
            <&> withRasterizedInfo (\_ p -> (point blue, rasterizedFragCoord p ^. _z - epsilon))

        let colorOption = ContextColorOption NoBlending (pure True)
            depthOption = DepthOption Less True
        drawWindowColorDepth (const (window, colorOption, depthOption)) fs

    return $ \viewport count -> do
        blockOutline <- toPrimitiveArrayIndexedInstanced LineList
            <$> newIndexArray blockOutlineIndexBuffer Nothing
            <*> return (,)
            <*> newVertexArray blockOutlineBuffer
            <*> (takeVertices count <$> newVertexArray outlineBuffer)
        shader (viewport, blockOutline)

createGridRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m)
    => Window os RGBAFloat Depth
    -> Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float))
    -> Buffer os (Uniform FogB)
    -> ContextT ctx os m (ViewPort -> Render os ())
createGridRenderer window projectionBuffer fogBuffer = do
    let grid =
            [ V3 1 1 0,  V3 (-1) 1 0,  V3 1 (-1) 0
            , V3 (-1) (-1) 0,  V3 1 (-1) 0,  V3 (-1) 1 0
            ]

    gridBuffer :: Buffer os (B3 Float) <- newBuffer (length grid)
    writeBuffer gridBuffer 0 grid

    shader :: CompiledShader os (ViewPort, PrimitiveArray Triangles (B3 Float))  <- compileShader $ do
        (projectionMat, cameraMat, camPos) <- getUniform (const (projectionBuffer, 0))
        let modelViewProj = projectionMat !*! cameraMat

        triangles :: PrimitiveStream Triangles (V3 VFloat) <- toPrimitiveStream snd
        let
            projectedTriangles :: PrimitiveStream Triangles (V4 VFloat, (V2 VFloat, VFloat))
            projectedTriangles =
                (\p -> (modelViewProj !* p, (p^._xy, camPos^._z))) .
                point.
                (* 4000) <$> triangles

            getRasterOptions (viewPort, _) = (FrontAndBack, viewPort, DepthRange 0 1)

        fog <- getUniform (const (fogBuffer, 0))
        fragCoord :: FragmentStream (V2 FFloat, FFloat) <- rasterize getRasterOptions projectedTriangles
        let
            drawGridLine :: (V2 FFloat, FFloat) -> V4 FFloat
            drawGridLine (p@(V2 x y), camPosZ) = color' where
                -- Pick a coordinate to visualize in a grid.
                (coord, coord') = (p / blockSize, p / blockSize / 10)
                -- Compute anti-aliased renderContext-space grid lines.
                V2 gx gy = abs (fract' (coord - 0.5) - 0.5) / (fwidth <$> coord)
                V2 gx' gy' = abs (fract' (coord' - 0.5) - 0.5) / (fwidth <$> coord')
                -- Change color when coord' and coord' match.
                (V3 r g b) =
                    ifThenElse' (abs x <* 0.5) (V3 0 1 0) -- ortho to X = Y => green
                        (ifThenElse' (abs y <* 0.5) (V3 1 0 0) -- ortho to Y = Z => red
                            (ifThenElse' (gx >* (gx' - 0.5) ||* gy >* (gy' - 0.5))
                                (pure 0.1)
                                (pure 0.3)))
                -- Just visualize the grid lines directly.
                line = minB gx gy
                color = V4 r g b (1 - minB line 1)
                -- Add fog.
                fogDistance = norm $ V3 (p^._x) (p^._y) camPosZ
                color' = applyFog fog color (fogDistance * 0.5)

            litFrags = drawGridLine <$> fragCoord

            litFragsWithDepth = withRasterizedInfo
                (\a p -> (a, rasterizedFragCoord p ^._z)) litFrags
            blending = BlendRgbAlpha
                (FuncAdd, FuncAdd)
                (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors Zero Zero)
                (point white)
            colorOption = ContextColorOption blending (pure True)
            depthOption = DepthOption Less True

        drawWindowColorDepth (const (window, colorOption, depthOption)) litFragsWithDepth

    return $ \viewPort -> do
        gridPrimArray <- toPrimitiveArray TriangleList <$> newVertexArray gridBuffer
        shader (viewPort, gridPrimArray)

createFrustumRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m)
    => Window os RGBAFloat Depth
    -> Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float))
    -> ContextT ctx os m (ViewPort -> Render os ())
createFrustumRenderer window projectionBuffer = do
    frustumBuffer :: Buffer os (B3 Float) <- newBuffer (length ndcFrustum)
    writeBuffer frustumBuffer 0 ndcFrustum

    let edges = concatMap (\(i, j) -> [i, j]) $ filter (\(i, j) -> j > i) cubeEdges
    frustumIndexBuffer :: Buffer os (BPacked Word8) <- newBuffer (length edges)
    writeBuffer frustumIndexBuffer 0 (map fromIntegral edges)

    shader :: CompiledShader os (ViewPort, PrimitiveArray Lines (B3 Float))  <- compileShader $ do
        (projectionMat1, cameraMat1, _) <- getUniform (const (projectionBuffer, 0))
        let modelViewProj = projectionMat1 !*! cameraMat1

        (projectionMat2, cameraMat2, _) <- getUniform (const (projectionBuffer, 1))
        let invModelViewProj = inv44 (projectionMat2 !*! cameraMat2)

        lines :: PrimitiveStream Lines (V3 VFloat) <- toPrimitiveStream snd
        let
            projectedLines :: PrimitiveStream Lines (V4 VFloat, ())
            projectedLines = (\p -> (modelViewProj !* (invModelViewProj !* p), ())) . point <$> lines

        let rasterOptions = \(viewPort, _) -> (Front, viewPort, DepthRange 0 1)
        fs :: FragmentStream (V4 FFloat, FragDepth) <- rasterize rasterOptions projectedLines
            <&> withRasterizedInfo (\_ p -> (point yellow, rasterizedFragCoord p ^. _z - epsilon))

        let colorOption = ContextColorOption NoBlending (pure True)
            depthOption = DepthOption Less True
        drawWindowColorDepth (const (window, colorOption, depthOption)) fs

    return $ \viewPort -> do
        frustum <- toPrimitiveArrayIndexed LineList
            <$> newIndexArray frustumIndexBuffer Nothing
            <*> newVertexArray frustumBuffer
        shader (viewPort, frustum)

-- Unused
bitsetToWord :: [Bool] -> Word32
bitsetToWord bx = assert (length bx <= 32) $
    sum (zipWith (\b i -> if b then 1 `shiftL` i else 0) bx [0..])

intToWord :: Word32 -> [Bool]
intToWord n = map (\i -> n .&. (1 `shiftL` i) > 0) (reverse [0..7])

{- Create a renderer for an infinite "landscape" produced by the calculateDensity
function.

glxinfo | egrep -i 'Currently available dedicated video memory'
-}
createPolygonisationRenderer :: (MonadIO m, MonadAsyncException m)
    => Window os RGBAFloat Depth
    -> ContextT GLFW.Handle os m (RenderContext m os)
createPolygonisationRenderer window = do
    let poolSize = 1000 -- 4 Mo / block
        blockBufferSize = blockSize^3 * 3
        indexBufferSize = blockBufferSize * maxCellTriangleCount
        vMemSize = blockBufferSize * 7 * 4 + indexBufferSize * 4

        formatBigNumber d =
            let (q, r) = d `quotRem` 1000
            in  if q > 0
                then formatBigNumber q ++ "," ++ printf "%03d" r
                else printf "%d" r

    liftIO $ infoM "Hadron" $ show poolSize ++ " allocated blocks (" ++ formatBigNumber vMemSize ++ " bytes)"

    pool :: [(Buffer os (B4 Float, B3 Float), Buffer os (B Word32))] <- replicateM poolSize $ (,) <$>
        newBuffer blockBufferSize <*>
        newBuffer indexBufferSize

    intersectionBuffer :: Buffer os (B Word32, V2 (B4 Float)) <- newBuffer $ blockSize^3

    offsetAndScaleBuffer :: Buffer os (Uniform (B3 Float, B Float)) <- newBuffer 1
    neighbourUpscaleTexture :: Texture3D os (Format RFloat) <- newTexture3D R8 (pure 3) 1
    densityTexture :: Texture3D os (Format RFloat) <- newTexture3D R16F (pure densityTextureSize) 1
    noiseTextures :: [Texture3D os (Format RFloat)] <- take 6 . cycle <$> replicateM 3 (generate3DNoiseTexture (16, 16, 16))

    fillDensityTexture <- createFillDensityRenderer offsetAndScaleBuffer neighbourUpscaleTexture densityTexture noiseTextures

    rayBuffer :: Buffer os (Uniform (B3 Float, B3 Float)) <- newBuffer 1

    generateIntersection <- createGenerateIntersectionRenderer window offsetAndScaleBuffer rayBuffer densityTexture noiseTextures

    let fillIntersectionBuffer scale neighbourUpscales offset ray = do
            writeBuffer offsetAndScaleBuffer 0 [(offset, scale)]
            writeBuffer rayBuffer 0 [ray]
            writeTexture3D neighbourUpscaleTexture 0 0 (pure 3) neighbourUpscales
            render $ fillDensityTexture >> generateIntersection intersectionBuffer
            Right count <- liftIO $ feedbackBufSize (undefined :: Points) intersectionBuffer
            let decodeValue (z8_y8_x8_case8, V2 (V4 d0 d1 d2 d3) (V4 d4 d5 d6 d7)) =
                    let [z, y, x, c] = decodeInHaskell [8, 8, 8, 8] z8_y8_x8_case8
                        p = offset + (fromIntegral <$> V3 x y z) ^* scale
                        densities = [d0, d1, d2, d3, d4, d5, d6, d7]
                    in ((scale, p), (intToWord c, densities))
            intersections <- map decodeValue <$> readBuffer intersectionBuffer 0 (fromIntegral count)
            let intersectionsAhead = intersections
                    & zip (map (getRayCoordinate ray . snd . fst) intersections)
                    & filter ((> 0) . fst) -- Unnecessary?
            return $ if null intersectionsAhead
                then Nothing
                else Just (snd (minimumBy (comparing fst) intersectionsAhead))

    generateBlock <- createGenerateBlockRenderer window offsetAndScaleBuffer densityTexture noiseTextures

    let fillIndexedBlock scale neighbourUpscales offset blockBuffer indexBuffer = do
            writeBuffer offsetAndScaleBuffer 0 [(offset, scale)]
            writeTexture3D neighbourUpscaleTexture 0 0 (pure 3) neighbourUpscales
            render $ fillDensityTexture >> generateBlock blockBuffer indexBuffer
            Right indexSize <- liftIO $ feedbackBufSize (undefined :: Triangles) indexBuffer
            return indexSize

    fogBuffer :: Buffer os (Uniform FogB) <- newBuffer 1
    sunBuffer :: Buffer os (Uniform DirectionLightB) <- newBuffer 1
    projectionBuffer :: Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float)) <- newBuffer 2 -- Two cameras.
    outlineBuffer :: Buffer os (B Float, B3 Float) <- newBuffer poolSize

    indexedBlockRenderer <- createIndexedBlockRenderer window projectionBuffer fogBuffer sunBuffer
    frustumRenderer <- createFrustumRenderer window projectionBuffer
    blockOutlinesRenderer <- createBlockOutlinesRenderer window projectionBuffer outlineBuffer
    gridRenderer <- createGridRenderer window projectionBuffer fogBuffer

    cellBuffer :: Buffer os (Uniform (B3 Float, B Word32)) <- newBuffer 1
    cubePreRenderer <- createCubePreRenderer window projectionBuffer fogBuffer sunBuffer

    -- skyBoxRenderer <- createSkyBoxRenderer window projectionBuffer

    let renderIt oldPickingRay cache _ bounds camera cameras sun lights _ _ gui cursor = do
            let ((x, y), (w, h)) = bounds
                debug = guiDebug gui
                picking = guiPicking gui
                fogDensity = guiFogDensity gui

            writeBuffer fogBuffer 0 [Fog (point skyBlue) 100 1000 fogDensity]
            writeBuffer sunBuffer 0 [sun]

            let firstCamera = head cameras
                otherCamera = head (filter (/= camera) cameras)

            writeBuffer projectionBuffer 0
                [ createProjection bounds camera
                , createProjection bounds otherCamera
                ]

            let blocks = take poolSize (listVisibleBlocks bounds firstCamera)

                pickingRay = if camera == firstCamera || true
                    then Just (calculatePickingRay (w, h) camera cursor)
                    else oldPickingRay

                generateIntersectionBlock _ _ [] = return Nothing
                generateIntersectionBlock i ray ((neighbourUpscales, (scale, offset)) : pbs) = do
                    pickingInfo <- fillIntersectionBuffer (scale / blockSize) neighbourUpscales offset ray
                    if isNothing pickingInfo
                        then generateIntersectionBlock (i + 1) ray pbs
                        else do
                            -- liftIO $ putStrLn $ "Picked " ++ show i ++ "th cube"
                            return pickingInfo

            -- let pickBlocks ray = map fst $ sortBy (comparing snd) $ mapMaybe (\b -> (,) b <$> rayWithCubeIntersect ray b) blocks
            let pickBlocks ray = map fst $ mapMaybe (\b -> (,) b <$> rayWithCubeIntersect ray b) blocks
                distanceToOrigin (o, _) (_, (s, p)) = let blockCenter = p + pure (s / 2) in distance p o
            maybeCubeRenderer <- if picking
                then case pickingRay of
                    Just ray -> do
                        generateIntersectionBlock 1 ray (sortOn (distanceToOrigin ray) (addNeighbourUpscaleToBlocks (pickBlocks ray))) >>= \case
                            Just cellPositionAndCaseWithDensities -> do
                                cubeRenderer <- cubePreRenderer cellPositionAndCaseWithDensities
                                return $ Just cubeRenderer
                            Nothing -> return Nothing
                    Nothing -> return Nothing
                else return Nothing

            let blocksWithUpscale = addNeighbourUpscaleToBlocks blocks
                generateIndexedBlock (neighbourUpscales, (scale, offset)) (blockBuffer, indexBuffer) = do
                    size <- fillIndexedBlock (scale / blockSize) neighbourUpscales offset blockBuffer indexBuffer
                    return $ if size > 0 then Just (blockBuffer, indexBuffer) else Nothing

            -- We cache the generated blocks (vertices + indices), using their bounding box (and neighbour upscales now) as a key.
            (cache', (blocks', indexedBlockBuffers)) <- second unzip <$> foldM
                (\(c, ds) k -> cacheLookup generateIndexedBlock c k >>= \(c', d) -> return (c', maybe ds (\d -> (k, d) : ds) d))
                (cacheUpdatePreserve cache blocksWithUpscale, [])
                blocksWithUpscale

            let highlightedBlocks = map snd blocks'
            when debug $
                writeBuffer outlineBuffer 0 highlightedBlocks

            render $ do
                clearWindowColor window (point skyBlue)
                clearWindowDepth window 1
                let viewPort = ViewPort (V2 x y) (V2 w h)
                -- skyBoxRenderer viewPort
                forM_ indexedBlockBuffers $ \indexedBlockBuffer ->
                    indexedBlockRenderer (viewPort, indexedBlockBuffer)
                when (isJust maybeCubeRenderer) $ do
                    fromJust maybeCubeRenderer viewPort
                when debug $ do
                    frustumRenderer viewPort
                    blockOutlinesRenderer viewPort (length highlightedBlocks)
                    -- gridRenderer viewPort

            return $ RenderContext Nothing (renderIt pickingRay cache')

    return $ RenderContext Nothing (renderIt Nothing (newCache pool))

-- ***

-- Break block ordering!
addNeighbourUpscaleToBlocks
    :: [(Float, V3 Float)] -- ^ A list of [(scale, lower corner)].
    -> [([Float], (Float, V3 Float))] -- ^ A list of [(neighbourUpscales, (scale, lower corner))].
addNeighbourUpscaleToBlocks blocks = f maxScale [] blocks where

    maxScale = maximum (map fst blocks)

    -- Vertices are ordered the same way as in a 3D texture.
    neighbourUpscaleOffsets = [ V3 dx dy dz | dz <- [-1, 0, 1], dy <- [-1, 0, 1], dx <- [-1, 0, 1] ]

    isPointInsideCube c (s, p) =
        p^._x < c^._x && c^._x < p^._x + s &&
        p^._y < c^._y && c^._y < p^._y + s &&
        p^._z < c^._z && c^._z < p^._z + s

    f _ _ [] = []
    f scale biggerBlocks blocks = map enhance sameBlocks ++ f (scale / 2) sameBlocks smallerBlocks where
        (sameBlocks, smallerBlocks) = partition (\b -> fst b == scale) blocks
        enhance block@(s, p) =
            let c = p + pure (s / 2)
                -- TODO Add a pass to merge upscales from faces in edges.
                neighbourUpscales = map getNeighbourUpscale neighbourUpscaleOffsets
                notACorner (V3 x y z) = abs x + abs y + abs z < 3 -- corner => single point with odd coordinates (no upscale)
                getNeighbourUpscale offset = if notACorner offset && any (isPointInsideCube (c + offset ^* s)) biggerBlocks then 1 else 0
            in  (neighbourUpscales, block)

{- List all visible blocks in a camera view frustum. The returned blocks are
just bounding boxes which need to be instanciated or retrieved from a cache (and
possibly discarded if empty).

Note: Using the lower corner (instead of the center) prevent any rounding errors
and allow us to use it as a key in the cache.
-}
listVisibleBlocks
    :: ((Int, Int), (Int, Int)) -- ^ Viewport bounds.
    -> Camera
    -> [(Float, V3 Float)] -- ^ A list of [(scale, lower corner)].
listVisibleBlocks bounds camera = allBlocks where
    (_, (w, h)) = bounds
    cameraPos = cameraPosition camera
    cameraMat = lookAt cameraPos (cameraPos + getSight camera) (getUp camera)

    projectionMat = perspective (cameraFov camera) (fromIntegral w / fromIntegral h) (cameraNear camera) (cameraFar camera)
    modelViewProj = projectionMat !*! cameraMat
    invModelViewProj = inv44 modelViewProj

    -- Frustum corners in normalized device coordinates.
    ndcFrustumCorners =
        [ V3 (-1) (-1) (-1)
        , V3 1 (-1) (-1)
        , V3 (-1) 1 (-1)
        , V3 1 1 (-1)
        , V3 (-1) (-1) 1
        , V3 1 (-1) 1
        , V3 (-1) 1 1
        , V3 1 1 1
        ]

    -- Same things in world coordinates.
    frustumCorners = map (normalizePoint . (invModelViewProj !*) . point) ndcFrustumCorners
    frustumIntersector = sphereWithFrustumIntersect frustumCorners

    -- distanceToSight p = -(cameraMat !* point p)^._z
    distanceToSight p = norm (cameraPos - p)

    maxLoadLevel = 2 -- 3
    size = blockSize * 2^maxLoadLevel

    blocks :: [V3 Float]
    blocks =
        let pieceWise f = foldl1 (\(V3 x1 y1 z1) (V3 x2 y2 z2) -> V3 (f x1 x2) (f y1 y2) (f z1 z2))
            -- The AABB whose coordinates are multiple of 'size' containing the frustum.
            V3 xMin yMin zMin :: V3 Int = fmap (\n -> floor (n / fromIntegral size) * size) (pieceWise min frustumCorners)
            V3 xMax yMax zMax :: V3 Int = fmap (\n -> ceiling (n / fromIntegral size) * size) (pieceWise max frustumCorners)
            r = pure (fromIntegral size / 2)
            cubeIntersectFrustum p = frustumIntersector (p + r) (norm r) /= Outside
        -- Divide it into blocks and filter out those outside the frustum.
        in  filter cubeIntersectFrustum
                [ fromIntegral <$> V3 x y z
                        | x <- [xMin, xMin+size .. xMax]
                        , y <- [yMin, yMin+size .. yMax]
                        , z <- [zMin, zMin+size .. zMax]
                        ]

    withSize s = map $ (,) (fromIntegral s)

    divide 0 s _ bs = withSize s bs
    divide depth s f bs =
        let s' = s `div` 2
            f' = f / 2 -- 1.5
            center b = b + pure (fromIntegral s')
            r = pure (fromIntegral s' / 2)
            cubeIntersectFrustum p = frustumIntersector (p + r) (norm r) /= Outside
            (farestBlocks, nearestBlocks) = partition (\b -> distanceToSight (center b) > f') bs
            subdivide b = [ b + (fromIntegral <$> (s' *^ V3 x y z)) | x <-[0, 1], y <-[0, 1], z <-[0, 1] ]
            subBlocks = filter cubeIntersectFrustum (concatMap subdivide nearestBlocks)
        in  divide (depth - 1) s' f' subBlocks ++ withSize s farestBlocks

    allBlocks :: [(Float, V3 Float)]
    allBlocks = divide maxLoadLevel size (cameraFar camera) blocks

-- ***

data Cache k v = Cache
    { cacheMap :: !(Map.Map k (Maybe v))
    , cacheReleasedKeys :: ![k]
    , cacheUnusedValues :: ![v]
    }

newCache :: [v] -> Cache k v
newCache = Cache Map.empty []

cacheUpdatePreserve :: Ord k
    => Cache k v
    -> [k] -- The keys which need to be preserved.
    -> Cache k v
cacheUpdatePreserve cache@(Cache m _ pool) ks = Cache m rks pool where
    rks = Set.toList (foldl' (flip Set.delete) (Set.fromList (Map.keys m)) ks)

{-
Note that 'Nothing' value are legitimate values and when is returned,
it will be cached but won’t consume an element from the pool.
-}
cacheLookup :: (Ord k, Monad m)
    => (k -> v -> m (Maybe v)) -- To generate the value if not in cache.
    -> Cache k v -- The cache.
    -> k -- The key.
    -> m (Cache k v,  Maybe v) -- The updated cache and the retrieved value.
cacheLookup f cache@(Cache m rks pool) k =
    case Map.lookup k m of
        Just v -> return (cache, v)
        Nothing ->
            if null pool
                then if null rks
                    then return (cache, Nothing)
                    else do
                        let freeEntries m [] = (m, [], Nothing)
                            freeEntries m (rk : rks) =
                                let mv = fromJust (Map.lookup rk m)
                                    m' = Map.delete rk m
                                in  case mv of
                                    Just v -> (m', rks, mv)
                                    Nothing -> freeEntries m' rks
                        let (m', rks', mv) = freeEntries m rks
                        case mv of
                            Nothing -> return (cache, Nothing)
                            Just v -> do
                                mv <- f k v
                                let pool' = if isNothing mv then v : pool else pool
                                    m'' = Map.insert k mv m'
                                return (Cache m'' rks' pool', mv)
                else do
                    mv <- f k (head pool)
                    let pool' = if isNothing mv then pool else tail pool
                        m' = Map.insert k mv m
                    return (Cache m' rks pool', mv)

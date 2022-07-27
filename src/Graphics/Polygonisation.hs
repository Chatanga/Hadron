{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}

module Graphics.Polygonisation
    ( createPolygonisationRenderer
    )
where

import Prelude hiding ((.), id, (<*))
import Control.Applicative (liftA2)
import Control.Category (Category((.)), id)
import Control.Lens ((&), (<&>), (^.), index)
import Control.Monad ( forM_, replicateM, forM, foldM, when )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Exception
import Data.Bits
import Data.Bifunctor (second)
-- import qualified Deque.Strict as Deque
import Data.Int (Int8, Int32)
import Data.List ((\\), partition, delete, foldl', find)
import Data.Maybe
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
import Graphics.Geometry
import Graphics.Intersection
import Graphics.MarchingCube
import Graphics.Shaders
import Graphics.Texture
import Control.Exception (assert)

----------------------------------------------------------------------------------------------------------------------

{-
GPipe translation of:
https://developer.nvidia.com/gpugems/gpugems3/part-i-geometry/chapter-1-generating-complex-procedural-terrains-using-gpu

What I’m missing:
- Indexed (fixed size) array.
- Bitwise operations to pack/unpack data efficiently.
-}

----------------------------------------------------------------------------------------------------------------------

forN :: (ShaderType a V) => (VInt, VInt) -> a -> (VInt -> a -> a) -> a
forN (i, n) x f = snd $ while
    (\(i, _) -> i <* n)
    (\(i, x) -> (i + 1, f i x))
    (i, x)

scalePoint :: (Num a) => V4 a -> a -> V4 a
scalePoint p s = point $ (p ^._xyz) ^* s

-- Normalized device coordinate
ndcFrustum :: Num a => [V3 a]
ndcFrustum =
    [ V3 (-1) (-1) (-1)
    , V3 1 (-1) (-1)
    , V3 (-1) 1 (-1)
    , V3 1 1 (-1)
    , V3 (-1) (-1) 1
    , V3 1 (-1) 1
    , V3 (-1) 1 1
    , V3 1 1 1
    ]

blockSize :: Num a => a
blockSize = 32

densityMargin :: Num a => a
densityMargin = 6

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
    base = p'^._z / 16
    -- base = (minB (norm p') (norm (p' + V3 40 0 0)) - 40) / 8
    -- base = (norm p' - 40) / 8
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

{- Create a renderer to fill the provided 3D density texture by sampling the
density function at the given offset and scale. The provided noise textures
are used by the density function.

    densityTexture(p) = density(offset + (p + densityMargin) * scale)

-}
createFillDensityRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m)
    => Buffer os (Uniform (B3 Float, B Float))
    -> Texture3D os (Format RFloat)
    -> Texture3D os (Format RFloat)
    -> [Texture3D os (Format RFloat)]
    -> ContextT ctx os m (Render os ())
createFillDensityRenderer offsetAndScaleBuffer neighbourUpscaleTexture densityTexture noiseTextures = do
    planeBuffer :: Buffer os (B2 Float) <- newBuffer 4
    writeBuffer planeBuffer 0 [V2 (-1) (-1), V2 1 (-1), V2 (-1) 1, V2 1 1]

    heightBuffer :: Buffer os (B Float) <- newBuffer densityTextureSize
    writeBuffer heightBuffer 0 (fromIntegral <$> [0 .. densityTextureSize - 1])

    shader :: CompiledShader os (Image (Format RFloat), PrimitiveArray Triangles (B Float, B2 Float))  <- compileShader $ do

        (offset, scale) <- getUniform (const (offsetAndScaleBuffer, 0))

        neighbourUpscaleSampler <- newSampler3D (const (neighbourUpscaleTexture, SamplerNearest, (pure ClampToEdge, undefined)))

        let filterMode = SamplerFilter Linear Linear Linear (Just 4)
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

        let fs' = getDensity' <$> fs

            getDensity :: V3 (S F Float) -> S F Float
            getDensity = calculateDensity 7 noiseSamplers (offset - scale *^ pure densityMargin) . (scale *^)

            getDensity' :: V3 FFloat -> FFloat
            getDensity' p =
                -- Pixel centers are located at half-pixel centers and need to be shifted (z is fine).
                -- (cf. https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_FragCoord.xhtml)
                let V3 x y z = p - V3 0.5 0.5 0
                    upscale = texelFetch3D neighbourUpscaleSampler (pure 0) 0 (toUnitaryVector (V3 x y z))
                    split c = let m = mod'' c 2 in  ifThenElse' (m <* 0.5) (c, c) (c - 1, c + 1)
                    (x1, x2) = split x
                    (y1, y2) = split y
                    (z1, z2) = split z
                    mean n ds = sum ds / n
                in  ifThenElse' (upscale >* 0.5)
                    (ifThenElse' (x1 /=* x2)
                        (ifThenElse' (y1 /=* y2)
                            (ifThenElse' (z1 /=* z2)
                                (mean 8 [ getDensity (V3 x y z) | x <- [x1, x2], y <- [y1, y2], z <- [z1, z2] ])
                                (mean 4 [ getDensity (V3 x y z) | x <- [x1, x2], y <- [y1, y2] ])
                            )
                            (ifThenElse' (z1 /=* z2)
                                (mean 4 [ getDensity (V3 x y z) | x <- [x1, x2], z <- [z1, z2] ])
                                (mean 2 [ getDensity (V3 x y z) | x <- [x1, x2] ])
                            )
                        )
                        (ifThenElse' (y1 /=* y2)
                            (ifThenElse' (z1 /=* z2)
                                (mean 4 [ getDensity (V3 x y z) | y <- [y1, y2], z <- [z1, z2] ])
                                (mean 2 [ getDensity (V3 x y z) | y <- [y1, y2] ])
                            )
                            (ifThenElse' (z1 /=* z2)
                                (mean 2 [ getDensity (V3 x y z) | z <- [z1, z2] ])
                                (mean 1 [ getDensity (V3 x y z) ])
                            )
                        )
                    )
                    (getDensity (V3 x y z))

            toUnitaryVector :: V3 FFloat -> V3 FInt
            toUnitaryVector (V3 x y z) =
                let toUnitary c = ifThenElse' (c <* densityMargin + 0.5) 0 (ifThenElse' (c >* densityMargin + blockSize - 0.5) 2 1)
                in  V3 (toUnitary x) (toUnitary y) (toUnitary z)

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

generateBoolCases :: Int -> [[Bool]]
generateBoolCases 0 = [[]]
generateBoolCases i = [ x:xs | x <- [False, True], xs <- generateBoolCases (i - 1) ]

-- 2^(7-i) benefits of being statically evaluated.
cellCaseFromDensities :: [VFloat] -> VInt
cellCaseFromDensities densities= foldl1 or' (zipWith (\i d -> ifB (d >* 0) (fromIntegral ((2^(7-i))::Int)) 0) [0..] densities)

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
    let extCellCount = (blockSize + 1) ^ 3

    extCellPositionBuffer :: Buffer os (B3 Int8) <- newBuffer extCellCount
    writeBuffer extCellPositionBuffer 0 [ fromIntegral <$> V3 x y z | x <- [0 .. blockSize], y <- [0 .. blockSize], z <- [0 .. blockSize] ]

    -- z8_y8_x8_case8
    let protoBlockBuffer1Size = extCellCount
    protoBlockBuffer1 :: Buffer os (B Word32) <- newBuffer protoBlockBuffer1Size

    listNonEmptyCells :: CompiledShader os (PrimitiveArray Points (B3 Int8))  <- compileShader $ do

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
        drawNothing window (const protoBlockBuffer1) Queried maxVertices gs'

    -- z8_y8_x8_edge8
    let protoBlockBuffer2Size = extCellCount * 3
    protoBlockBuffer2 :: Buffer os (B Word32) <- newBuffer protoBlockBuffer2Size

    listVertToGenerateShader :: CompiledShader os (Buffer os (B Word32), PrimitiveArray Points (B Word32))  <- compileShader $ do

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
        drawNothing window (const protoBlockBuffer2) Queried maxVertices gs'

    cubeVerticeTexture :: Texture1D os (Format RGBInt) <- newTexture1D RGB8I (length cube) 1
    writeTexture1D cubeVerticeTexture 0 0 (length cube) (cube :: [V3 Int8])

    let poissonOnSphere = map (\(x, y, z) -> V3 x y z) Random.poissonOnSphere
            {-
            [ (-1, 0, 0)
            , (1, 0, 0)
            , (0, -1, 0)
            , (0, 1, 0)
            , (0, 0, -1)
            , (0, 0, 1)
            ]
            -}

    poissonOnSphereTexture :: Texture1D os (Format RGBFloat) <- newTexture1D RGB16F (length poissonOnSphere) 1
    writeTexture1D poissonOnSphereTexture 0 0 (length poissonOnSphere) poissonOnSphere

    genVerticesShader :: CompiledShader os (Buffer os (B4 Float, B3 Float), (Buffer os (B Word32), PrimitiveArray Points (B Word32)))  <- compileShader $ do

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
                        sample dv = getFloatDensity (v + dv)
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
        drawNothing window fst Queried maxVertices gs'

    let size = blockSize + 1
    splatVertexIdsTexture :: Texture3D os (Format RWord) <- newTexture3D R32UI (V3 (3 * size) size size) 1

    splatVertexIdsShader :: CompiledShader os (Image (Format RWord), (Buffer os (B Word32), PrimitiveArray Points (B Word32)))  <- compileShader $ do

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

    casesTexture :: Texture2D os (Format RGBInt) <- newTexture2D RGB8I (V2 maxCellTriangleCount 256) 1
    writeTexture2D casesTexture 0 0 (V2 maxCellTriangleCount 256) (concatMap toPaddedCaseContent protoTriangleLists)

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

    genIndicesShader :: CompiledShader os (Buffer os (B Word32), (Buffer os (B Word32), PrimitiveArray Points (B Word32)))  <- compileShader $ do

        aritySampler <- newSampler1D (const (arityTexture, SamplerNearest, (ClampToEdge, undefined)))
        caseSampler <- newSampler2D (const (casesTexture, SamplerNearest, (pure ClampToEdge, undefined)))
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
        positions <- toPrimitiveArray PointList <$> newVertexArray extCellPositionBuffer
        listNonEmptyCells positions
        protoBlocks1 <- toPrimitiveArray PointList <$> newVertexArray protoBlockBuffer1
        listVertToGenerateShader (protoBlockBuffer1, protoBlocks1)
        protoBlocks2 <- toPrimitiveArray PointList <$> newVertexArray protoBlockBuffer2
        genVerticesShader (blockBuffer, (protoBlockBuffer2, protoBlocks2))
        splatVertexIds <- getLayeredTextureImage splatVertexIdsTexture 0
        splatVertexIdsShader (splatVertexIds, (protoBlockBuffer2, protoBlocks2))
        genIndicesShader (indexBuffer, (protoBlockBuffer1, protoBlocks1))

createIndexedBlockRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m)
    => Window os RGBAFloat Depth
    -> Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float))
    -> Buffer os (Uniform FogB)
    -> Buffer os (Uniform DirectionLightB)
    -> ContextT ctx os m ((ViewPort, (Buffer os (B4 Float, B3 Float), Buffer os (B Word32))) -> Render os ())
createIndexedBlockRenderer window projectionBuffer fogBuffer sunBuffer = do

    noiseTexture :: Texture2D os (Format RFloat) <- generate2DNoiseTexture (16, 16)

    Just altMap <- loadImage "data/altmap.tga"
    generateTexture2DMipmap altMap

    shader :: CompiledShader os (ViewPort, (Buffer os (B Word32), PrimitiveArray Triangles (B4 Float, B3 Float)))  <- compileShader $ do

        (projectionMat, cameraMat, cameraPos) <- getUniform (const (projectionBuffer, 0))
        let modelViewProj = projectionMat !*! cameraMat

        ps :: primitiveStream Triangles (V4 VFloat, V3 VFloat) <- toFeedbackPrimitiveStream (fst . snd) (snd . snd)
        let ps' :: primitiveStream Triangles (VPos, (V4 VFloat, V3 VFloat, V3 VFloat)) = ps <&> \(po, n) -> (modelViewProj !* point (po ^. _xyz), (po, n, cameraPos))

        fog :: FogS F <- getUniform (const (fogBuffer, 0))
        sun :: DirectionLightS F <- getUniform (const (sunBuffer, 0))

        noiseSampler <- newSampler2D (const (noiseTexture, SamplerFilter Linear Linear Linear (Just 4), (pure Mirror, undefined)))
        altMapSampler <- newSampler2D (const (altMap, SamplerFilter Linear Linear Linear (Just 4), (pure Mirror, undefined)))

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
                    sample p = sample2D noiseSampler (SampleLod 0) Nothing Nothing (p / 1000)
                    uv = V2 (sample $ p^. _xy) (p^. _z / 64 + 32)
                    material = sample2D altMapSampler SampleAuto Nothing Nothing uv
                    m = point $ material ^* saturate (1 - o)
                    -- m = point 0.5
                    c = getSunlight undefined n Nothing p m 1 lightingContext
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
createBlockOutlinesRenderer window projectionBuffer offsetBuffer = do

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
            <&> withRasterizedInfo (\_ p -> (point blue, rasterizedFragCoord p ^. _z - 0.0000001))

        let colorOption = ContextColorOption NoBlending (pure True)
            depthOption = DepthOption Less True
        drawWindowColorDepth (const (window, colorOption, depthOption)) fs

    return $ \viewport count -> do
        blockOutline <- toPrimitiveArrayIndexedInstanced LineList
            <$> newIndexArray blockOutlineIndexBuffer Nothing
            <*> return (,)
            <*> newVertexArray blockOutlineBuffer
            <*> (takeVertices count <$> newVertexArray offsetBuffer)
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
                    ifThenElse' (abs x <* 0.5) (V3 1 0 0)
                        (ifThenElse' (abs y <* 0.5) (V3 0 1 0)
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
            <&> withRasterizedInfo (\_ p -> (point yellow, rasterizedFragCoord p ^. _z - 0.0000001))

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

{- Create a renderer for an infinite "landscape" produced by the calculateDensity
function.

glxinfo | egrep -i 'Currently available dedicated video memory'
-}
createPolygonisationRenderer :: (MonadIO m, MonadAsyncException m)
    => Window os RGBAFloat Depth
    -> ContextT GLFW.Handle os m (RenderContext m os)
createPolygonisationRenderer window = do
    let debug = False
        poolSize = 1000
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

    offsetAndScaleBuffer :: Buffer os (Uniform (B3 Float, B Float)) <- newBuffer 1
    neighbourUpscaleTexture :: Texture3D os (Format RFloat) <- newTexture3D R8 (pure 3) 1
    densityTexture :: Texture3D os (Format RFloat) <- newTexture3D R16F (pure densityTextureSize) 1
    noiseTextures :: [Texture3D os (Format RFloat)] <- take 6 . cycle <$> replicateM 3 (generate3DNoiseTexture (16, 16, 16))

    fillDensityTexture <- createFillDensityRenderer offsetAndScaleBuffer neighbourUpscaleTexture densityTexture noiseTextures
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

    writeBuffer fogBuffer 0 [Fog (point skyBlue) 100 1000 0.002]

    let renderIt cache _ bounds camera cameras sun lights buffers normalBuffers = do
            let ((x, y), (w, h)) = bounds

            writeBuffer sunBuffer 0 [sun]

            let otherCamera = head (filter (/= camera) cameras)

            writeBuffer projectionBuffer 0
                [ createProjection bounds camera
                , createProjection bounds otherCamera
                ]

            let blocks = addNeighbourUpscaleToBlocks (take poolSize (listVisibleBlocks bounds (head cameras)))

            let generateIndexedBlock (neighbourUpscales, (scale, offset)) (blockBuffer, indexBuffer) = do
                    size <- fillIndexedBlock (scale / blockSize) neighbourUpscales offset blockBuffer indexBuffer
                    return $ if size > 0 then Just (blockBuffer, indexBuffer) else Nothing

            -- We cache the generated blocks (vertices + indices), using their bounding box (and neighbour upscales now) as a key.
            (cache', (blocks', indexedBlockBuffers)) <- second unzip <$> foldM
                (\(c, ds) k -> cacheLookup generateIndexedBlock c k >>= \(c', d) -> return (c', maybe ds (\d -> (k, d) : ds) d))
                (cacheUpdatePreserve cache blocks, [])
                blocks

            when debug $ writeBuffer outlineBuffer 0 (map snd blocks')

            render $ do
                clearWindowColor window (point skyBlue)
                clearWindowDepth window 1
                let viewPort = ViewPort (V2 x y) (V2 w h)
                forM_ indexedBlockBuffers $ \indexedBlockBuffer ->
                    indexedBlockRenderer (viewPort, indexedBlockBuffer)
                when debug $ do
                    -- frustumRenderer viewPort
                    blockOutlinesRenderer viewPort (length blocks')
                    -- gridRenderer viewPort

            return $ RenderContext Nothing (renderIt cache')

    return $ RenderContext Nothing (renderIt (newCache pool))

-- ***

addNeighbourUpscaleToBlocks
    :: [(Float, V3 Float)] -- ^ A list of [(scale, lower corner)].
    -> [([Float], (Float, V3 Float))] -- ^ A list of [(neighbourUpscales, (scale, lower corner))].
addNeighbourUpscaleToBlocks blocks = f maxScale [] blocks where

    maxScale = maximum (map fst blocks)

    -- Vertices are ordered the same way as in a 3D texture.
    neighbourUpscaleOffsets = [ V3 dx dy dz | dz <- [-1, 0, 1], dy <- [-1, 0, 1], dx <- [-1, 0, 1] ]

    isPointInsideCube c (s, p) =
        c^._x > p^._x && c^._x < p^._x + s &&
        c^._y > p^._y && c^._y < p^._y + s &&
        c^._z > p^._z && c^._z < p^._z + s

    f _ _ [] = []
    f scale biggerBlocks blocks = map enhance sameBlocks ++ f (scale / 2) sameBlocks smallerBlocks where
        (sameBlocks, smallerBlocks) = partition (\b -> fst b == scale) blocks
        enhance block@(s, p) =
            let c = p + pure (s / 2)
                neighbourUpscales = map getNeighbourUpscale neighbourUpscaleOffsets
                getNeighbourUpscale offset = if any (isPointInsideCube (c + offset ^* s)) biggerBlocks then 1 else 0
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

    maxLoadLevel = 2
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
            f' = f / 2
            center b = b + pure (fromIntegral s')
            r = pure (fromIntegral s' / 2)
            cubeIntersectFrustum p = frustumIntersector (p + r) (norm r) /= Outside
            (farestBlocks, nearestBlocks) = partition (\b -> distanceToSight (center b) > f') bs
            subdivide b = [ b + (fromIntegral <$> (s' *^ V3 x y z)) | x <-[0..1], y <-[0..1], z <-[0..1] ]
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
Note that 'Nothing' value are legitimate values and  is returned,
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

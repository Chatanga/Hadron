{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}

module Graphics.Polygonisation
    ( createPolygonisationRenderer
    )
where

import Prelude hiding ((.), id, (<*))
import Control.Applicative (liftA2)
import Control.Category (Category((.)), id)
import Control.Lens ((&), (<&>), (^.), index)
import Control.Monad ( forM_, replicateM, forM, foldM )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Exception
import Data.Bits
import Data.Bifunctor (second)
import qualified Deque.Strict as Deque
import Data.Int (Int8, Int32)
import Data.List ((\\), partition, delete, foldl')
import Data.Maybe
import qualified Data.Map.Strict as Map
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

createFillDensityRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) =>
    Buffer os (Uniform (B3 Float, B Float)) ->
    Texture3D os (Format RFloat) ->
    [Texture3D os (Format RFloat)] ->
    ContextT ctx os m (Render os ())
createFillDensityRenderer offsetAndScaleBuffer densityTexture noiseTextures = do
    let (V3 size _ _) = head (texture3DSizes densityTexture)

    planeBuffer :: Buffer os (B2 Float) <- newBuffer 4
    writeBuffer planeBuffer 0 [V2 (-1) (-1), V2 1 (-1), V2 (-1) 1, V2 1 1]

    heightBuffer :: Buffer os (B Float) <- newBuffer size
    writeBuffer heightBuffer 0 (fromIntegral <$> [0 .. size - 1])

    shader :: CompiledShader os (Image (Format RFloat), PrimitiveArray Triangles (B Float, B2 Float))  <- compileShader $ do

        (offset, scale) <- getUniform (const (offsetAndScaleBuffer, 0))

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

        let rasterOptions = const (Front, ViewPort 0 (pure size), DepthRange 0 1)
            maxVertices = 3
        fs <- generateAndRasterize rasterOptions maxVertices gs'
            <&> withRasterizedInfo (\h info -> let V4 x y _ _ = rasterizedFragCoord info in V3 x y h)

        let fs' = calculateDensity 7 noiseSamplers (offset - pure densityMargin ^* scale) . (scale *^) <$> fs

        draw (const NoBlending) fs' $
            drawColor (\env -> (fst env, True, False))

    return $ do
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

createGenerateBlockRenderer1 :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) =>
    Window os RGBAFloat Depth ->
    Buffer os (Uniform (B3 Float, B Float)) ->
    Texture3D os (Format RFloat) ->
    ContextT ctx os m (Buffer os (B3 Float, B3 Float) -> Render os ())
createGenerateBlockRenderer1 window offsetBufferAndScale densityTexture = do
    let cellCount = blockSize ^ 3

    cellPositionBuffer :: Buffer os (B3 Int8) <- newBuffer cellCount
    writeBuffer cellPositionBuffer 0 [ fromIntegral <$> V3 x y z | x <- [0 .. blockSize-1], y <- [0 .. blockSize-1], z <- [0 .. blockSize-1] ]

    let verticeCount = maxCellTriangleCount * 3 * 2 -- 3 edges (of 2 vertices) per triangle.

    let protoTriangleLists = map generateCaseProtoTriangleList (generateBoolCases 8)
        toCaseContent :: [(EdgeIndice, EdgeIndice, EdgeIndice)] -> [V3 Int8] -- We only need a [0, 1] domain actually…
        toCaseContent protoTriangleList = concatMap (\(e1, e2, e3) -> concatMap ((\(i1, i2) -> map (cube !!) [i1, i2]). (cubeEdges !!)) [e1, e2, e3]) protoTriangleList
        toPaddedCaseContent protoTriangleList = take verticeCount (toCaseContent protoTriangleList ++ repeat (V3 0 0 0))

    arityTexture :: Texture1D os (Format RInt) <- newTexture1D R8I 256 1
    writeTexture1D arityTexture 0 0 256 (map (fromIntegral . length) protoTriangleLists :: [Int32])

    casesTexture :: Texture2D os (Format RGBInt) <- newTexture2D RGB8I (V2 verticeCount 256) 1
    writeTexture2D casesTexture 0 0 (V2 verticeCount 256) (concatMap toPaddedCaseContent protoTriangleLists)

    shader :: CompiledShader os (Buffer os (B3 Float, B3 Float), PrimitiveArray Points (B3 Int8))  <- compileShader $ do

        (offset, scale) <- getUniform (const (offsetBufferAndScale, 0))

        -- FIXME Since the 2 samplers use the same texture, the last attributes will prevail (GPipe doesn't use sampler object).
        -- It prevents us to have a nice linear density for normals and occlusion.
        floatDensitySampler <- newSampler3D (const (densityTexture, SamplerFilter Linear Linear Linear (Just 4), (pure ClampToEdge, undefined)))
        densitySampler <- newSampler3D (const (densityTexture, SamplerNearest, (pure ClampToEdge, undefined)))

        aritySampler <- newSampler1D (const (arityTexture, SamplerNearest, (ClampToEdge, undefined)))
        caseSampler <- newSampler2D (const (casesTexture, SamplerNearest, (pure ClampToEdge, undefined)))

        ps :: primitiveStream Points (V3 VInt) <- toPrimitiveStream snd

        gs :: GeometryStream (Geometry Points (V3 VInt)) <- geometrize ps

        let makeTriangles :: Geometry Points (V3 VInt) -> GGenerativeGeometry Triangles (V3 VFloat, V3 VFloat)
            makeTriangles (Point p) = triangles
                where
                    getFloatDensity :: V3 VFloat -> VFloat
                    getFloatDensity v =
                        let uv = (v + pure densityMargin + pure 0.5) / (blockSize + densityMargin * 2 + 1)
                        in  sample3D floatDensitySampler (SampleLod 0) Nothing Nothing uv

                    getDensity :: V3 VInt -> VFloat
                    getDensity = texelFetch3D densitySampler (pure 0) 0 . (+ pure densityMargin)

                    densities :: [VFloat]
                    densities = map getDensity $ fmap (p +) cube

                    getArity :: VInt -> VInt
                    getArity = texelFetch1D aritySampler (pure 0) 0

                    getCase :: V2 VInt -> V3 VInt
                    getCase = texelFetch2D caseSampler (pure 0) 0

                    cellCase = cellCaseFromDensities densities

                    count = getArity cellCase

                    triangles :: GGenerativeGeometry Triangles (V3 VFloat, V3 VFloat)
                    triangles = forN (0, count) generativeTriangleStrip emitTriangle

                    getNormal :: V3 VFloat -> V3 VFloat
                    getNormal v = normal where
                        sample dv = getFloatDensity (v + dv)
                        grad = V3
                            (sample (V3 1 0 0) - sample (V3 (-1) 0 0))
                            (sample (V3 0 1 0) - sample (V3 0 (-1) 0))
                            (sample (V3 0 0 1) - sample (V3 0 0 (-1)))
                        normal = signorm grad

                    emitTriangle i =
                        let [a0, b0, a1, b1, a2, b2] = [ p + getCase (V2 (i * 6 + fromIntegral o) cellCase) | o <- [0..5] ]
                            calculatePoint :: V3 VInt -> V3 VInt -> V3 VFloat
                            calculatePoint v1 v2 = (1 - a) *^ (toFloat <$> v1) + a *^ (toFloat <$> v2) where
                                (d1, d2) = (getDensity v1, getDensity v2)
                                a = d1 / (d1 - d2)
                            p1 = calculatePoint a0 b0
                            p2 = calculatePoint a1 b1
                            p3 = calculatePoint a2 b2
                        in  endPrimitive .
                            emitVertex (offset + p1 ^* scale, getNormal p1) .
                            emitVertex (offset + p3 ^* scale, getNormal p3) .
                            emitVertex (offset + p2 ^* scale, getNormal p2)

            gs' :: GeometryStream (GGenerativeGeometry Triangles (V3 VFloat, V3 VFloat)) = gs <&> makeTriangles

        let maxVertices = maxCellTriangleCount * 3
        drawNothing window fst Queried maxVertices gs'

    return $ \blockBuffer -> do
        positions <- toPrimitiveArray PointList <$> newVertexArray cellPositionBuffer
        shader (blockBuffer, positions)

createGenerateBlockRenderer2 :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) =>
    Window os RGBAFloat Depth ->
    Buffer os (Uniform (B3 Float, B Float)) ->
    Texture3D os (Format RFloat) ->
    ContextT ctx os m (Buffer os (B3 Float, B3 Float) -> Render os ())
createGenerateBlockRenderer2 window offsetAndScaleBuffer densityTexture = do
    let cellCount = blockSize ^ 3

    cellPositionBuffer :: Buffer os (B3 Int8) <- newBuffer cellCount
    writeBuffer cellPositionBuffer 0 [ fromIntegral <$> V3 x y z | x <- [0 .. blockSize-1], y <- [0 .. blockSize-1], z <- [0 .. blockSize-1] ]

    -- z5_y5_x5_edge5_edge5_edge5
    let protoBlockBufferSize = cellCount * maxCellTriangleCount
    protoBlockBuffer :: Buffer os (B Word32) <- newBuffer protoBlockBufferSize

    let protoTriangleLists = map generateCaseProtoTriangleList (generateBoolCases 8)
        toCaseContent :: [(EdgeIndice, EdgeIndice, EdgeIndice)] -> [V3 Int8]
        toCaseContent protoTriangleList = map (\(e1, e2, e3) -> fromIntegral <$> V3 e1 e2 e3) protoTriangleList
        toPaddedCaseContent protoTriangleList = take maxCellTriangleCount (toCaseContent protoTriangleList ++ repeat (V3 0 0 0))

    arityTexture :: Texture1D os (Format RInt) <- newTexture1D R8I 256 1
    writeTexture1D arityTexture 0 0 256 (map (fromIntegral . length) protoTriangleLists :: [Int32])

    casesTexture :: Texture2D os (Format RGBInt) <- newTexture2D RGB8I (V2 maxCellTriangleCount 256) 1
    writeTexture2D casesTexture 0 0 (V2 maxCellTriangleCount 256) (concatMap toPaddedCaseContent protoTriangleLists)

    listTrianglesShader :: CompiledShader os (PrimitiveArray Points (B3 Int8))  <- compileShader $ do

        densitySampler <- newSampler3D (const (densityTexture, SamplerNearest, (pure ClampToEdge, undefined)))
        aritySampler <- newSampler1D (const (arityTexture, SamplerNearest, (ClampToEdge, undefined)))
        caseSampler <- newSampler2D (const (casesTexture, SamplerNearest, (pure ClampToEdge, undefined)))

        ps :: primitiveStream Points (V3 VInt) <- toPrimitiveStream id

        gs :: GeometryStream (Geometry Points (V3 VInt)) <- geometrize ps

        let makeProtoTriangles :: Geometry Points (V3 VInt) -> GGenerativeGeometry Points VWord
            makeProtoTriangles (Point p) = protoTriangles
                where
                    getDensity :: V3 VInt -> VFloat
                    getDensity = texelFetch3D densitySampler (pure 0) 0 . (+ pure densityMargin)

                    densities :: [VFloat]
                    densities = map (getDensity . (p +)) cube

                    getArity :: VInt -> VInt
                    getArity = texelFetch1D aritySampler (pure 0) 0

                    getCase :: V2 VInt -> V3 VInt
                    getCase = texelFetch2D caseSampler (pure 0) 0

                    cellCase = cellCaseFromDensities densities

                    count = getArity cellCase

                    protoTriangles :: GGenerativeGeometry Points VWord
                    protoTriangles = forN (0, count) generativePoints emitProtoTriangle

                    emitProtoTriangle :: VInt -> GGenerativeGeometry Points VWord -> GGenerativeGeometry Points VWord
                    emitProtoTriangle i =
                        let V3 x y z = toWord <$> p
                            V3 e1 e2 e3 = toWord <$> getCase (V2 i cellCase)
                            -- GPU Gems code use a "z6_y6_x6_edge1_edge2_edge3" format which is weird.
                            -- 6 bit aren’t needed for [0, blockSize] position and 4 bits is not enough for a directed edges indice.
                            z5_y5_x5_edge5_edge5_edge5 = encode [5, 5, 5, 5, 5, 5] [z, y, x, e1, e2, e3]
                        in  endPrimitive . emitVertex z5_y5_x5_edge5_edge5_edge5 -- TODO Group endPrimitive calls?

            gs' :: GeometryStream (GGenerativeGeometry Points VWord) = makeProtoTriangles <$> gs

        let maxVertices = maxCellTriangleCount
        drawNothing window (const protoBlockBuffer) Queried maxVertices gs'

    cubeEdgeTexture :: Texture2D os (Format RInt) <- newTexture2D R8I (V2 2 (length cubeEdges)) 1
    writeTexture2D cubeEdgeTexture 0 0 (V2 2 (length cubeEdges)) (fromIntegral <$> concatMap (\(i, j) -> [i, j]) cubeEdges :: [Int32])

    cubeVerticeTexture :: Texture1D os (Format RGBInt) <- newTexture1D RGB8I (length cube) 1
    writeTexture1D cubeVerticeTexture 0 0 (length cube) (cube :: [V3 Int8])

    genVerticesShader :: CompiledShader os (Buffer os (B3 Float, B3 Float), (Buffer os (B Word32), PrimitiveArray Points (B Word32)))  <- compileShader $ do

        (offset, scale) <- getUniform (const (offsetAndScaleBuffer, 0))

        -- FIXME Since the 2 samplers use the same texture, the last attributes will prevail (GPipe doesn't use sampler object)..
        -- It prevents us to have a nice linear density for normals and occlusion.
        floatDensitySampler <- newSampler3D (const (densityTexture, SamplerFilter Linear Linear Linear (Just 4), (pure ClampToEdge, undefined)))
        densitySampler <- newSampler3D (const (densityTexture, SamplerNearest, (pure ClampToEdge, undefined)))

        cubeEdgeSampler <- newSampler2D (const (cubeEdgeTexture, SamplerNearest, (pure ClampToEdge, undefined)))
        cubeVerticeSampler <- newSampler1D (const (cubeVerticeTexture, SamplerNearest, (ClampToEdge, undefined)))

        ps :: primitiveStream Points VWord <- toFeedbackPrimitiveStream (fst . snd) (snd . snd)

        let toTriangles :: VWord -> V3 (V3 VFloat, V3 VFloat)
            toTriangles z5_y5_x5_edge5_edge5_edge5 = triangles
                where
                    [z, y, x, e1, e2, e3] = map toInt $ decode [5, 5, 5, 5, 5, 5] z5_y5_x5_edge5_edge5_edge5
                    p = V3 x y z

                    getFloatDensity :: V3 VFloat -> VFloat
                    getFloatDensity v =
                        let uv = (v + pure densityMargin + pure 0.5) / (blockSize + densityMargin * 2 + 1)
                        in  sample3D floatDensitySampler (SampleLod 0) Nothing Nothing uv

                    getDensity :: V3 VInt -> VFloat
                    getDensity = texelFetch3D densitySampler (pure 0) 0 . (+ pure densityMargin)

                    getCubeEdge :: VInt -> (VInt, VInt)
                    getCubeEdge i =
                        ( texelFetch2D cubeEdgeSampler (pure 0) 0 (V2 0 i)
                        , texelFetch2D cubeEdgeSampler (pure 0) 0 (V2 1 i))

                    getCubeVertice :: VInt -> V3 VInt
                    getCubeVertice i = texelFetch1D cubeVerticeSampler (pure 0) 0 i

                    triangles :: V3 (V3 VFloat, V3 VFloat)
                    triangles = V3 (createTriangle e1) (createTriangle e3) (createTriangle e2)

                    getNormal :: V3 VFloat -> V3 VFloat
                    getNormal v = normal where
                        sample dv = getFloatDensity (v + dv)
                        grad = V3
                            (sample (V3 1 0 0) - sample (V3 (-1) 0 0))
                            (sample (V3 0 1 0) - sample (V3 0 (-1) 0))
                            (sample (V3 0 0 1) - sample (V3 0 0 (-1)))
                        normal = signorm grad

                    createTriangle e =
                        let (i, j) = getCubeEdge e
                            v1 = p + getCubeVertice i
                            v2 = p + getCubeVertice j
                            calculatePoint :: V3 VInt -> V3 VInt -> V3 VFloat
                            calculatePoint v1 v2 = (1 - a) *^ (toFloat <$> v1) + a *^ (toFloat <$> v2) where
                                (d1, d2) = (getDensity v1, getDensity v2)
                                a = d1 / (d1 - d2)
                            v = calculatePoint v1 v2
                        in  (offset + v ^* scale, getNormal v)

        let ps' :: primitiveStream Points (V3 (V3 VFloat, V3 VFloat)) = toTriangles <$> ps

        gs :: GeometryStream (Geometry Points (V3 (V3 VFloat, V3 VFloat))) <- geometrize ps'

        let makeTriangle :: Geometry Points (V3 (V3 VFloat, V3 VFloat)) -> GGenerativeGeometry Triangles (V3 VFloat, V3 VFloat)
            makeTriangle (Point (V3 (p1, n1) (p2, n2) (p3, n3))) = generativeTriangleStrip
                & emitVertex (p2, n2)
                & emitVertex (p3, n3)
                & emitVertex (p1, n1)
                & endPrimitive

            gs' :: GeometryStream (GGenerativeGeometry Triangles (V3 VFloat, V3 VFloat)) = makeTriangle <$> gs

        let maxVertices = 3
        drawNothing window fst Queried maxVertices gs'

    return $ \blockBuffer -> do
        positions <- toPrimitiveArray PointList <$> newVertexArray cellPositionBuffer
        listTrianglesShader positions
        -- TODO Should be nice to be able to abort here on an empty list.
        protoBlocks <- toPrimitiveArray PointList <$> newVertexArray protoBlockBuffer
        genVerticesShader (blockBuffer, (protoBlockBuffer, protoBlocks))

createGenerateBlockRenderer3 :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) =>
    Window os RGBAFloat Depth ->
    Buffer os (Uniform (B3 Float, B Float)) ->
    Texture3D os (Format RFloat) ->
    [Texture3D os (Format RFloat)] ->
    ContextT ctx os m (Buffer os (B4 Float, B3 Float) -> Buffer os (B Word32) -> Render os ())
createGenerateBlockRenderer3 window offsetAndScaleBuffer densityTexture noiseTextures = do
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

        let filterMode = SamplerFilter Linear Linear Linear (Just 4)
            edgeMode = (pure Repeat, undefined)
        noiseSamplers <- forM noiseTextures $ \t -> newSampler3D (const (t, filterMode, edgeMode))

        -- FIXME Since the 2 samplers use the same texture, the last attributes will prevail (GPipe doesn't use sampler object)..
        -- It prevents us to have a nice linear density for normals and occlusion.
        floatDensitySampler <- newSampler3D (const (densityTexture, SamplerFilter Linear Linear Linear (Just 4), (pure ClampToEdge, undefined)))
        densitySampler <- newSampler3D (const (densityTexture, SamplerNearest, (pure ClampToEdge, undefined)))

        cubeVerticeSampler <- newSampler1D (const (cubeVerticeTexture, SamplerNearest, (ClampToEdge, undefined)))

        ps :: primitiveStream Points VWord <- toFeedbackPrimitiveStream (fst . snd) (snd . snd)

        let toVertice :: VWord -> (V4 VFloat, V3 VFloat)
            toVertice z8_y8_x8_edge8 = vertice
                where
                    [z, y, x, verticeIndice] = map toInt $ decode [8, 8, 8, 8] z8_y8_x8_edge8
                    p = V3 x y z

                    getFloatDensity :: V3 VFloat -> VFloat
                    getFloatDensity v =
                        let uv = (v + pure densityMargin + pure 0.5) / (blockSize + densityMargin * 2 + 1)
                        in  sample3D floatDensitySampler (SampleLod 0) Nothing Nothing uv

                    -- Long range implied.
                    calculateFloatDensity :: V3 VFloat -> VFloat
                    calculateFloatDensity v = calculateDensity 6 noiseSamplers offset (v ^* scale + V3 0.5 0.5 0.0) -- TODO Investigate (layered texture?).

                    getDensity :: V3 VInt -> VFloat
                    getDensity = texelFetch3D densitySampler (pure 0) 0 . (+ pure densityMargin)

                    getCubeVertice :: VInt -> V3 VInt
                    getCubeVertice = texelFetch1D cubeVerticeSampler (pure 0) 0

                    vertice :: (V4 VFloat, V3 VFloat)
                    vertice = createVertice verticeIndice

                    getNormal :: V3 VFloat -> V3 VFloat
                    getNormal v = normal where
                        sample dv = getFloatDensity (v + dv + pure 0.5)
                        grad = V3
                            (sample (V3 1 0 0) - sample (V3 (-1) 0 0))
                            (sample (V3 0 1 0) - sample (V3 0 (-1) 0))
                            (sample (V3 0 0 1) - sample (V3 0 0 (-1)))
                        normal = signorm grad

                    getRayDir :: VInt -> V3 VFloat
                    getRayDir = texelFetch1D raySampler (pure 0) 0

                    -- TODO Take scale into account.
                    calculateOcclusion :: V3 VFloat -> VFloat
                    calculateOcclusion v =
                        let rayCount = fromIntegral (length poissonOnSphere)
                            visibility = forN (0, rayCount) 0 (\ray visibility ->
                                let dir = getRayDir ray
                                    shortRangeRayVisibility = forN (1, 4) 1 (\step rayVisibility ->
                                        rayVisibility * saturate (getFloatDensity (v + dir ^* toFloat step) * 999)) -- 8
                                    longRangeRayVisibility = forN (1, 4) 1 (\step rayVisibility ->
                                        rayVisibility * saturate (calculateFloatDensity (v + dir ^* (32 * toFloat step)) * 9999)) -- 0.5
                                in  visibility + shortRangeRayVisibility * longRangeRayVisibility
                                )
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

createBlockRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) =>
    Window os RGBAFloat Depth ->
    Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float)) ->
    Buffer os (Uniform FogB) ->
    Buffer os (Uniform DirectionLightB) ->
    ContextT ctx os m ((ViewPort, Buffer os (B3 Float, B3 Float)) -> Render os ())
createBlockRenderer window projectionBuffer fogBuffer sunBuffer = do
    shader :: CompiledShader os (ViewPort, (Buffer os (B3 Float, B3 Float), PrimitiveArray Triangles (B3 Float, B3 Float)))  <- compileShader $ do

        (projectionMat, cameraMat, cameraPos) <- getUniform (const (projectionBuffer, 0))
        let modelViewProj = projectionMat !*! cameraMat

        ps :: primitiveStream Triangles (V3 VFloat, V3 VFloat) <- toFeedbackPrimitiveStream (fst . snd) (snd . snd)
        let ps' :: primitiveStream Triangles (VPos, (V3 VFloat, V3 VFloat, V3 VFloat)) = ps <&> \(p, n) -> (modelViewProj !* point p, (p, n, cameraPos))

        fog :: FogS F <- getUniform (const (fogBuffer, 0))
        sun :: DirectionLightS F <- getUniform (const (sunBuffer, 0))

        let rasterOptions = \(viewPort, _) -> (FrontAndBack, viewPort, DepthRange 0 1)
        fs :: FragmentStream (V4 FFloat, FragDepth) <- rasterize rasterOptions ps' <&> withRasterizedInfo (\(p, n, cp) ri ->
                let lightingContext =
                        ( cp
                        , fog
                        , sun
                        , 0.8 -- material specular intensity
                        , 8 -- material specular power
                        )
                    material = V4 1 0.1 0.2 1
                    c = getSunlight undefined n Nothing p material 1 lightingContext
                in  (c, rasterizedFragCoord ri ^. _z))

        let colorOption = ContextColorOption NoBlending (pure True)
            depthOption = DepthOption Less True
        drawWindowColorDepth (const (window, colorOption, depthOption)) fs

    return $ \(viewPort, blockBuffer) -> do
        block <- toPrimitiveArray TriangleList <$> newVertexArray blockBuffer
        shader (viewPort, (blockBuffer, block))

createIndexedBlockRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) =>
    Window os RGBAFloat Depth ->
    Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float)) ->
    Buffer os (Uniform FogB) ->
    Buffer os (Uniform DirectionLightB) ->
    ContextT ctx os m ((ViewPort, (Buffer os (B4 Float, B3 Float), Buffer os (B Word32))) -> Render os ())
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

createBlockOutlineRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) =>
    Window os RGBAFloat Depth ->
    Buffer os (Uniform (B3 Float, B Float)) ->
    Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float)) ->
    ContextT ctx os m (ViewPort -> Render os ())
createBlockOutlineRenderer window offsetAndScaleBuffer projectionBuffer = do

    blockOutlineBuffer :: Buffer os (B3 Float) <- newBuffer (length cube)
    writeBuffer blockOutlineBuffer 0 cube

    let edges = concatMap (\(i, j) -> [i, j]) $ filter (\(i, j) -> j > i) cubeEdges
    blockOutlineIndexBuffer :: Buffer os (BPacked Word8) <- newBuffer (length edges)
    writeBuffer blockOutlineIndexBuffer 0 (map fromIntegral edges)

    shader :: CompiledShader os (ViewPort, (Int, PrimitiveArray Lines (B3 Float)))  <- compileShader $ do
        (projectionMat, cameraMat, _) <- getUniform (const (projectionBuffer, 0))
        let modelViewProj = projectionMat !*! cameraMat

        (offset, scale) <- getUniform (\env -> (offsetAndScaleBuffer, fst (snd env)))

        lines :: PrimitiveStream Lines (V3 VFloat) <- toPrimitiveStream (snd . snd)
        let
            projectedLines :: PrimitiveStream Lines (V4 VFloat, ())
            projectedLines =
                (\p -> (modelViewProj !* p, ())) .
                (\p -> point (p ^* scale + offset)) .
                (* blockSize) <$>
                lines

        let rasterOptions = \(viewPort, _) -> (Front, viewPort, DepthRange 0 1)
        fs :: FragmentStream (V4 FFloat, FragDepth) <- rasterize rasterOptions projectedLines
            <&> withRasterizedInfo (\_ p -> (point white, rasterizedFragCoord p ^. _z - 0.0000001))

        let colorOption = ContextColorOption NoBlending (pure True)
            depthOption = DepthOption Less True
        drawWindowColorDepth (const (window, colorOption, depthOption)) fs

    return $ \viewPort -> do
        blockOutline <- toPrimitiveArrayIndexed LineList
            <$> newIndexArray blockOutlineIndexBuffer Nothing
            <*> newVertexArray blockOutlineBuffer
        shader (viewPort, (0, blockOutline))

createBlockOutlinesRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) =>
    Window os RGBAFloat Depth ->
    Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float)) ->
    Buffer os (B Float, B3 Float) ->
    ContextT ctx os m (ViewPort -> Int -> Render os ())
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

createGridRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) =>
    Window os RGBAFloat Depth ->
    Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float)) ->
    Buffer os (Uniform FogB) ->
    ContextT ctx os m (ViewPort -> Render os ())
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

createFrustumRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) =>
    Window os RGBAFloat Depth ->
    Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float)) ->
    ContextT ctx os m (ViewPort -> Render os ())
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

generateBlocks :: (MonadIO m, MonadAsyncException m) =>
    Window os RGBAFloat Depth ->
    Float ->
    ContextT GLFW.Handle os m [Buffer os (B3 Float, B3 Float)]
generateBlocks window scale = do
    let blockBufferSize = blockSize^3 * maxCellTriangleCount * 3

        offsets = [ (* (blockSize * scale)) <$> V3 x y z |
            -- x <- [0], y <- [0], z <- [0]
            x <- [-3 .. 2], y <- [-3 .. 2], z <- [-1 .. 2]
            -- x <- [-6 .. 5], y <- [-6 .. 5], z <- [-4 .. 3]
            -- x <- [-8 .. 7], y <- [-8 .. 7], z <- [-4 .. 3]
            ]

    offsetAndScaleBuffer :: Buffer os (Uniform (B3 Float, B Float)) <- newBuffer 1
    densityTexture :: Texture3D os (Format RFloat) <- newTexture3D R16F (pure (blockSize + densityMargin * 2 + 1)) 1
    noiseTextures :: [Texture3D os (Format RFloat)] <- take 6 . cycle <$> replicateM 3 (generate3DNoiseTexture (16, 16, 16))

    fillDensityTexture <- createFillDensityRenderer offsetAndScaleBuffer densityTexture noiseTextures
    generateBlock <- createGenerateBlockRenderer2 window offsetAndScaleBuffer densityTexture

    let generateCellFromOffset offset blockBuffer = do
            writeBuffer offsetAndScaleBuffer 0 [(offset, scale)]
            render $ fillDensityTexture >> generateBlock blockBuffer
            blockSize <- liftIO $ feedbackBufSize (undefined :: Triangles) blockBuffer
            liftIO $ debugM "Hadron" $ "block size: " ++ show blockSize

    blockBuffers :: [Buffer os (B3 Float, B3 Float)] <- replicateM (length offsets) (newBuffer blockBufferSize)

    -- Creating blocks.
    forM_ (zip offsets blockBuffers) $ \(offset, blockBuffer) -> do
        generateCellFromOffset offset blockBuffer

    return blockBuffers

generateIndexedBlocks :: (MonadIO m, MonadAsyncException m) =>
    Window os RGBAFloat Depth ->
    Float ->
    ContextT GLFW.Handle os m [(Buffer os (B4 Float, B3 Float), Buffer os (B Word32))]
generateIndexedBlocks window scale = do
    let blockBufferSize = blockSize^3 * 3
        indexBufferSize = blockBufferSize * maxCellTriangleCount

        offsets = [ (* (blockSize * scale)) <$> V3 x y z |
            -- x <- [-1, 0], y <- [-1, 0], z <- [0]
            -- x <- [-3 .. 2], y <- [-3 .. 2], z <- [-1 .. 2]
            x <- [-6 .. 5], y <- [-6 .. 5], z <- [-4 .. 3]
            -- x <- [-8 .. 7], y <- [-8 .. 7], z <- [-4 .. 3]
            ]

    offsetAndScaleBuffer :: Buffer os (Uniform (B3 Float, B Float)) <- newBuffer 1
    densityTexture :: Texture3D os (Format RFloat) <- newTexture3D R16F (pure (blockSize + densityMargin * 2 + 1)) 1
    noiseTextures :: [Texture3D os (Format RFloat)] <- take 6 . cycle <$> replicateM 3 (generate3DNoiseTexture (16, 16, 16))

    fillDensityTexture <- createFillDensityRenderer offsetAndScaleBuffer densityTexture noiseTextures
    generateBlock <- createGenerateBlockRenderer3 window offsetAndScaleBuffer densityTexture noiseTextures

    let generateCellFromOffset offset blockBuffer indexBuffer = do
            writeBuffer offsetAndScaleBuffer 0 [(offset, scale)]
            render $ fillDensityTexture >> generateBlock blockBuffer indexBuffer
            blockSize <- liftIO $ feedbackBufSize (undefined :: Triangles) blockBuffer
            indexSize <- liftIO $ feedbackBufSize (undefined :: Triangles) indexBuffer
            -- liftIO $ debugM "Hadron" $ "block size: " ++ show blockSize ++ " / index size: " ++ show indexSize
            return ()

    blockBuffers :: [Buffer os (B4 Float, B3 Float)] <- replicateM (length offsets) (newBuffer blockBufferSize)
    indexBuffers :: [Buffer os (B Word32)] <- replicateM (length offsets) (newBuffer indexBufferSize)

    -- Creating blocks.
    forM_ (zip3 offsets blockBuffers indexBuffers) $ \(offset, blockBuffer, indexBuffer) -> do
        generateCellFromOffset offset blockBuffer indexBuffer

    return $ zip blockBuffers indexBuffers

-- glxinfo | egrep -i 'Currently available dedicated video memory'
createPolygonisationRenderer1 :: (MonadIO m, MonadAsyncException m) =>
    Window os RGBAFloat Depth ->
    ContextT GLFW.Handle os m (RenderContext m os)
createPolygonisationRenderer1 window = do
    let scale = 1

    -- blockBuffers :: [Buffer os (B3 Float, B3 Float)] <- generateBlocks window scale
    indexedBlockBuffers :: [(Buffer os (B4 Float, B3 Float), Buffer os (B Word32))] <- generateIndexedBlocks window scale

    fogBuffer :: Buffer os (Uniform FogB) <- newBuffer 1
    sunBuffer :: Buffer os (Uniform DirectionLightB) <- newBuffer 1
    projectionBuffer :: Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float)) <- newBuffer 2
    offsetAndScaleBuffer :: Buffer os (Uniform (B3 Float, B Float)) <- newBuffer 1
    outlineBuffer :: Buffer os (B Float, B3 Float) <- newBuffer 400

    -- blockRenderer <- createBlockRenderer window projectionBuffer fogBuffer sunBuffer
    indexedBlockRenderer <- createIndexedBlockRenderer window projectionBuffer fogBuffer sunBuffer
    blockOutlineRenderer <- createBlockOutlineRenderer window offsetAndScaleBuffer projectionBuffer
    blockOutlinesRenderer <- createBlockOutlinesRenderer window projectionBuffer outlineBuffer
    gridRenderer <- createGridRenderer window projectionBuffer fogBuffer
    frustumRenderer <- createFrustumRenderer window projectionBuffer

    writeBuffer fogBuffer 0 [Fog (point skyBlue) 10 100 0.2]
    writeBuffer offsetAndScaleBuffer 0 [(pure 0, scale)]

    let cachedBlocks :: Map.Map (Float, V3 Float) (Buffer os (B4 Float, B3 Float), Buffer os (B Word32)) = Map.empty

    let renderIt _ bounds camera cameras sun lights buffers normalBuffers = do
            let ((x, y), (w, h)) = bounds

            writeBuffer sunBuffer 0 [sun]

            let otherCamera = head (filter (/= camera) cameras)

            writeBuffer projectionBuffer 0
                [ createProjection bounds camera
                , createProjection bounds otherCamera
                ]

            let blocks = take (bufferLength outlineBuffer) (selectBlocks bounds (head cameras))
            writeBuffer outlineBuffer 0 blocks

            render $ do
                clearWindowColor window (point skyBlue)
                clearWindowDepth window 1
                let viewPort = ViewPort (V2 x y) (V2 w h)
                -- forM_ blockBuffers $ \blockBuffer -> blockRenderer (viewPort, blockBuffer)
                forM_ indexedBlockBuffers $ \indexedBlockBuffer -> indexedBlockRenderer (viewPort, indexedBlockBuffer)
                blockOutlineRenderer viewPort
                frustumRenderer viewPort
                blockOutlinesRenderer viewPort (length blocks)
                gridRenderer viewPort

            return $ RenderContext Nothing renderIt

    return (RenderContext Nothing renderIt)

createPolygonisationRenderer :: (MonadIO m, MonadAsyncException m) =>
    Window os RGBAFloat Depth ->
    ContextT GLFW.Handle os m (RenderContext m os)
createPolygonisationRenderer window = do
    let poolSize = 1000
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
    densityTexture :: Texture3D os (Format RFloat) <- newTexture3D R16F (pure (blockSize + densityMargin * 2 + 1)) 1
    noiseTextures :: [Texture3D os (Format RFloat)] <- take 6 . cycle <$> replicateM 3 (generate3DNoiseTexture (16, 16, 16))

    fillDensityTexture <- createFillDensityRenderer offsetAndScaleBuffer densityTexture noiseTextures
    generateBlock <- createGenerateBlockRenderer3 window offsetAndScaleBuffer densityTexture noiseTextures

    let fillIndexedBlock scale offset blockBuffer indexBuffer = do
            writeBuffer offsetAndScaleBuffer 0 [(offset, scale)]
            render $ fillDensityTexture >> generateBlock blockBuffer indexBuffer
            Right indexSize <- liftIO $ feedbackBufSize (undefined :: Triangles) indexBuffer
            return indexSize

    fogBuffer :: Buffer os (Uniform FogB) <- newBuffer 1
    sunBuffer :: Buffer os (Uniform DirectionLightB) <- newBuffer 1
    projectionBuffer :: Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float)) <- newBuffer 2
    outlineBuffer :: Buffer os (B Float, B3 Float) <- newBuffer poolSize

    indexedBlockRenderer <- createIndexedBlockRenderer window projectionBuffer fogBuffer sunBuffer
    blockOutlinesRenderer <- createBlockOutlinesRenderer window projectionBuffer outlineBuffer
    gridRenderer <- createGridRenderer window projectionBuffer fogBuffer
    frustumRenderer <- createFrustumRenderer window projectionBuffer

    writeBuffer fogBuffer 0 [Fog (point skyBlue) 100 1000 0.002]

    let renderIt cache _ bounds camera cameras sun lights buffers normalBuffers = do
            let ((x, y), (w, h)) = bounds

            writeBuffer sunBuffer 0 [sun]

            let otherCamera = head (filter (/= camera) cameras)

            writeBuffer projectionBuffer 0
                [ createProjection bounds camera
                , createProjection bounds otherCamera
                ]

            let blocks = take (bufferLength outlineBuffer) (selectBlocks bounds (head cameras))

            let gen (scale, offset) (blockBuffer, indexBuffer) = do
                    size <- fillIndexedBlock (scale / blockSize) offset blockBuffer indexBuffer
                    return $ if size > 0 then Just (blockBuffer, indexBuffer) else Nothing

            (cache', (blocks', indexedBlockBuffers)) <- second unzip <$> foldM
                (\(c, ds) k -> cacheLookup gen c k >>= \(c', d) -> return (c', maybe ds (\d -> (k, d) : ds) d))
                (cache, [])
                blocks

            -- writeBuffer outlineBuffer 0 blocks'

            render $ do
                clearWindowColor window (point skyBlue)
                clearWindowDepth window 1
                let viewPort = ViewPort (V2 x y) (V2 w h)
                forM_ indexedBlockBuffers $ \indexedBlockBuffer ->
                    indexedBlockRenderer (viewPort, indexedBlockBuffer)
                -- blockOutlineRenderer viewPort
                -- frustumRenderer viewPort
                -- blockOutlinesRenderer viewPort (length blocks')
                -- gridRenderer viewPort

            return $ RenderContext Nothing (renderIt cache')

    return $ RenderContext Nothing (renderIt (newCache pool))

data Cache k v = Cache
    { cacheMap :: !(Map.Map k (Maybe v))
    , cacheInnerKeys :: !(Deque.Deque k)
    , cacheOuterValues :: ![v]
    }

newCache :: [v] -> Cache k v
newCache = Cache Map.empty (fromList [])

cacheLookup :: (Ord k, Monad m) => (k -> v -> m (Maybe v)) -> Cache k v -> k -> m (Cache k v,  Maybe v)
cacheLookup f cache@(Cache m iks pool) k =
    case Map.lookup k m of
        Just v -> return (cache, v)
        Nothing ->
            if null pool
                then if null iks
                    then return (cache, Nothing)
                    else do
                        let (ksNothing, ksJust) = Deque.span (\k -> (isNothing . fromJust) (Map.lookup k m)) iks
                            Just (k', ks) = Deque.uncons ksJust
                            v' = fromJust . fromJust $ Map.lookup k' m
                        v <- f k v'
                        let pool' = [v' | isNothing v] -- A HLint suggestion, but rather hermetic I think.
                            m' = Map.insert k v $ foldl' (flip Map.delete) m (k' : toList ksNothing)
                            iks' = Deque.snoc k ks
                        return (Cache m' iks' pool', v)
                else do
                    v <- f k (head pool)
                    let pool' = if isNothing v then pool else tail pool
                        m' = Map.insert k v m
                        iks' = Deque.snoc k iks
                    return (Cache m' iks' pool', v)

selectBlocks :: ((Int, Int), (Int, Int)) -> Camera -> [(Float, V3 Float)]
selectBlocks bounds camera = allBlocks where
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

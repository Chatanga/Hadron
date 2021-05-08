{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}

module Graphics.Polygonisation
    ( createPolygonisationRenderer
    )
where

import Prelude hiding ((.), id, (<*))
import Control.Applicative (liftA2)
import Control.Category (Category((.)), id)
import Control.Lens ((&), (<&>), (^.))
import Control.Monad ( forM_, replicateM, forM )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Exception (MonadException)
import Data.Int (Int8, Int32)
import Data.List ((\\), partition)
import Data.Word (Word8, Word16, Word32)
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW

import Common.Debug

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

blockSize :: Num a => a
blockSize = 32

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

createFillDensityRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) =>
    Buffer os (Uniform (B3 Float)) ->
    Texture3D os (Format RFloat) ->
    [Texture3D os (Format RFloat)] ->
    ContextT ctx os m (Render os ())
createFillDensityRenderer offsetBuffer densityTexture noiseTextures = do
    let (V3 size _ _) = head (texture3DSizes densityTexture)

    planeBuffer :: Buffer os (B2 Float) <- newBuffer 6
    writeBuffer planeBuffer 0 [V2 (-1) (-1), V2 1 (-1), V2 1 1, V2 1 1, V2 (-1) 1, V2 (-1) (-1)]

    heightBuffer :: Buffer os (B Float) <- newBuffer size
    writeBuffer heightBuffer 0 (fromIntegral <$> [0 .. size - 1])

    shader :: CompiledShader os (Image (Format RFloat), PrimitiveArray Triangles (B Float, B2 Float))  <- compileShader $ do

        offset@(V3 dx dy dz) <- getUniform (const (offsetBuffer, 0))

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
        fs <- generateAndRasterize rasterOptions 3 gs'
            <&> withRasterizedInfo (\h info -> let V4 x y _ _ = rasterizedFragCoord info in V3 x y h)

        -- TODO Bug in my geometry shader support: not using every varying will lead to a crash.

        let toV3 x y z = x*x + y*y + z*z - 100
            fs'old = (\(V3 x y z) -> toV3 (x + dx - 10) (y + dy + 3) (z + dz - 10)) <$> fs

        let sample i p a b = sample3D (noiseSamplers !! i) SampleAuto Nothing Nothing (p * a / 256) * b
            f p =
                let p' = p + offset
                    density = p'^._z / blockSize
                    -- density = (norm p' - 40) / 8
                in  density
                    + sample 0 p' 4.03 0.25
                    + sample 1 p' 1.96 0.50
                    + sample 2 p' 1.01 1.00
                    + sample 3 p' 0.80 1.25
                    + sample 4 p' 0.50 1.50
                    + sample 5 p' 0.10 2.00
            fs' = f <$> fs

        draw (const NoBlending) fs' $
            drawColor (\env -> (fst env, True, False))

    return $ do
        plane <- newVertexArray planeBuffer
        height <- newVertexArray heightBuffer
        let toV3 :: B2 Float -> B Float -> (B Float, B2 Float)
            toV3 i b = (b, i)
        let primitiveArray = toPrimitiveArrayInstanced TriangleList toV3 plane height
        image <- getLayeredTextureImage densityTexture 0
        shader (image, primitiveArray)

generateCase :: Int -> [Int]
generateCase 0 = [0]
generateCase i = [ x + xs | x <- [0, 2^(i - 1)], xs <- generateCase (i - 1) ]

generateBoolCase :: Int -> [[Bool]]
generateBoolCase 0 = [[]]
generateBoolCase i = [ x:xs | x <- [False, True], xs <- generateBoolCase (i - 1) ]

createGenerateBlockRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) =>
    Window os RGBAFloat Depth ->
    Buffer os (Uniform (B3 Float)) ->
    Texture3D os (Format RFloat) ->
    ContextT ctx os m (Buffer os (B3 Float, B3 Float) -> Render os ())
createGenerateBlockRenderer window offsetBuffer densityTexture = do
    let (V3 size _ _) = head (texture3DSizes densityTexture)
        cellCount = (size - 1) ^ (3 :: Int)

    let protoBlockBufferSize = cellCount * maxCellTriangleCount
    protoBlockBuffer :: Buffer os (B Word32) <- newBuffer protoBlockBufferSize

    cellPositionBuffer :: Buffer os (B3 Int8) <- newBuffer cellCount
    writeBuffer cellPositionBuffer 0 [ fromIntegral <$> V3 x y z | x <- [0 .. size-2], y <- [0 .. size-2], z <- [0 .. size-2] ]

    let protoTriangleLists = map generateCaseProtoTriangleList (generateBoolCase 8)
        toCaseContent :: [(EdgeIndice, EdgeIndice, EdgeIndice)] -> [V3 Int8]
        toCaseContent protoTriangleList = map (\(e1, e2, e3) -> fromIntegral <$> V3 e1 e2 e3) protoTriangleList
        toPaddedCaseContent protoTriangleList = take maxCellTriangleCount (toCaseContent protoTriangleList ++ repeat (V3 0 0 0))

    arityTexture :: Texture1D os (Format RInt) <- newTexture1D R8I 256 1
    writeTexture1D arityTexture 0 0 256 (map (fromIntegral . length) protoTriangleLists :: [Int32])

    casesTexture :: Texture2D os (Format RGBInt) <- newTexture2D RGB8I (V2 maxCellTriangleCount 256) 1
    writeTexture2D casesTexture 0 0 (V2 maxCellTriangleCount 256) (concatMap toPaddedCaseContent protoTriangleLists)

    listVerticesShader :: CompiledShader os (PrimitiveArray Points (B3 Int8))  <- compileShader $ do

        densitySampler <- newSampler3D (const (densityTexture, SamplerNearest, (pure ClampToEdge, undefined)))
        aritySampler <- newSampler1D (const (arityTexture, SamplerNearest, (ClampToEdge, undefined)))
        caseSampler <- newSampler2D (const (casesTexture, SamplerNearest, (pure ClampToEdge, undefined)))

        ps :: primitiveStream Points (V3 VInt) <- toPrimitiveStream id

        gs :: GeometryStream (Geometry Points (V3 VInt)) <- geometrize ps

        let makeProtoTriangles :: Geometry Points (V3 VInt) -> GGenerativeGeometry Points VWord
            makeProtoTriangles (Point p) = protoTriangles
                where
                    getDensity :: V3 VInt -> VFloat
                    getDensity = texelFetch3D densitySampler (pure 0) 0

                    densities :: [VFloat]
                    densities = map (getDensity . (p +)) cube

                    getArity :: VInt -> VInt
                    getArity = texelFetch1D aritySampler (pure 0) 0

                    getCase :: V2 VInt -> V3 VInt
                    getCase = texelFetch2D caseSampler (pure 0) 0

                    -- 2^(7-i) benefits of being statically evaluated.
                    cellCase :: VInt
                    cellCase = foldl1 or' (zipWith (\i d -> ifB (d >* 0) (fromIntegral ((2^(7-i))::Int)) 0) [0..] densities)

                    count = getArity cellCase

                    protoTriangles :: GGenerativeGeometry Points VWord
                    (_, protoTriangles) = while
                        (\(i, _) -> i <* count)
                        (\(i, gg) -> (i+1, emitProtoTriangle i gg))
                        (0, generativePoints)

                    emitProtoTriangle :: VInt -> GGenerativeGeometry Points VWord -> GGenerativeGeometry Points VWord
                    emitProtoTriangle i =
                        let V3 x y z = toWord <$> p
                            V3 e1 e2 e3 = toWord <$> getCase (V2 i cellCase)
                            -- GPU Gems code use a "z6_y6_x6_edge1_edge2_edge3" format which is weird.
                            -- 6 bit aren’t needed for [0, blockSize] position and 4 bits is not enough for a directed edges indice.
                            z5_y5_x5_edge5_edge5_edge5 = foldl1 or' (zipWith shiftL' [z, y, x, e1, e2, e3] [25, 20, 15, 10, 5, 0])
                        in  endPrimitive . emitVertex z5_y5_x5_edge5_edge5_edge5 -- TODO Group endPrimitive calls?

            gs' :: GeometryStream (GGenerativeGeometry Points VWord) = makeProtoTriangles <$> gs

        let maxVertices = maxCellTriangleCount
        drawNothing window (const protoBlockBuffer) maxVertices gs'

    cubeEdgeTexture :: Texture2D os (Format RInt) <- newTexture2D R8I (V2 2 (length cubeEdges)) 1
    writeTexture2D cubeEdgeTexture 0 0 (V2 2 (length cubeEdges)) (fromIntegral <$> concatMap (\(i, j) -> [i, j]) cubeEdges :: [Int32])

    cubeVerticeSampler :: Texture1D os (Format RGBInt) <- newTexture1D RGB8I (length cube) 1
    writeTexture1D cubeVerticeSampler 0 0 (length cube) (cube :: [V3 Int8])

    genVerticesShader :: CompiledShader os (Buffer os (B3 Float, B3 Float), (Buffer os (B Word32), PrimitiveArray Points (B Word32)))  <- compileShader $ do

        offset <- getUniform (const (offsetBuffer, 0))

        floatDensitySampler <- newSampler3D (const (densityTexture, SamplerFilter Linear Linear Linear (Just 4), (pure ClampToEdge, undefined)))

        densitySampler <- newSampler3D (const (densityTexture, SamplerNearest, (pure ClampToEdge, undefined)))
        cubeEdgeSampler <- newSampler2D (const (cubeEdgeTexture, SamplerNearest, (pure ClampToEdge, undefined)))
        cubeVerticeSampler <- newSampler1D (const (cubeVerticeSampler, SamplerNearest, (ClampToEdge, undefined)))

        ps :: primitiveStream Points VWord <- toPrimitiveStream' (Just (fst . snd)) (snd . snd)

        let toTriangles :: VWord -> V3 (V3 VFloat, V3 VFloat)
            toTriangles z5_y5_x5_edge5_edge5_edge5 = triangles
                where
                    extract position size = toInt (and' (shiftR' z5_y5_x5_edge5_edge5_edge5 position) (2^size-1))
                    [z, y, x, e1, e2, e3] :: [VInt] = zipWith extract [25, 20, 15, 10, 5, 0] [5, 5, 5, 5, 5, 5]
                    p = V3 x y z

                    getDensity :: V3 VInt -> VFloat
                    getDensity = texelFetch3D densitySampler (pure 0) 0

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
                        sample = sample3D floatDensitySampler (SampleLod 0) Nothing Nothing . (/ blockSize) . (v +)
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
                                a = maxB 0 . minB 1 $ d1 / (d1 - d2)
                            v = calculatePoint v1 v2
                        in  (offset + v, getNormal v)

        let ps' :: primitiveStream Points (V3 (V3 VFloat, V3 VFloat)) = toTriangles <$> ps

        gs :: GeometryStream (Geometry Points (V3 (V3 VFloat, V3 VFloat))) <- geometrize ps'

        -- Any extension for pure pass-through GS in OpenGL?
        let makeTriangles :: Geometry Points (V3 (V3 VFloat, V3 VFloat)) -> GGenerativeGeometry Triangles (V3 VFloat, V3 VFloat)
            makeTriangles (Point (V3 (p1, n1) (p2, n2) (p3, n3))) = generativeTriangleStrip
                & emitVertex (p2, n2)
                & emitVertex (p3, n3)
                & emitVertex (p1, n1)
                & endPrimitive

            gs' :: GeometryStream (GGenerativeGeometry Triangles (V3 VFloat, V3 VFloat)) = gs <&> makeTriangles

        let maxVertices = 3
        drawNothing window fst maxVertices gs'

    return $ \blockBuffer -> do
        positions <- toPrimitiveArray PointList <$> newVertexArray cellPositionBuffer
        listVerticesShader positions
        protoBlocks <- toPrimitiveArray PointList <$> newVertexArray protoBlockBuffer
        genVerticesShader (blockBuffer, (protoBlockBuffer, protoBlocks))

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

        ps :: primitiveStream Triangles (V3 VFloat, V3 VFloat) <- toPrimitiveStream' (Just (fst . snd)) (snd . snd)
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
        -- What is the cost of calling toPrimitiveArray for each block?
        block <- toPrimitiveArray TriangleList <$> newVertexArray blockBuffer
        shader (viewPort, (blockBuffer, block))

createBlockOutlineRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) =>
    Window os RGBAFloat Depth ->
    Buffer os (Uniform (B3 Float)) ->
    Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float)) ->
    ContextT ctx os m (ViewPort -> Render os ())
createBlockOutlineRenderer window offsetBuffer projectionBuffer = do

    blockOutlineBuffer :: Buffer os (B3 Float) <- newBuffer (length cube)
    writeBuffer blockOutlineBuffer 0 cube

    let edges = concatMap (\(i, j) -> [i, j]) $ filter (\(i, j) -> j > i) cubeEdges
    blockOutlineIndexBuffer :: Buffer os (BPacked Word8) <- newBuffer (length edges)
    writeBuffer blockOutlineIndexBuffer 0 (map fromIntegral edges)

    shader :: CompiledShader os (ViewPort, (Int, PrimitiveArray Lines (B3 Float)))  <- compileShader $ do
        (projectionMat, cameraMat, _) <- getUniform (const (projectionBuffer, 0))
        let modelViewProj = projectionMat !*! cameraMat

        offset <- getUniform (\env -> (offsetBuffer, fst (snd env)))

        lines :: PrimitiveStream Lines (V3 VFloat) <- toPrimitiveStream (snd . snd)
        let
            projectedLines :: PrimitiveStream Lines (V4 VFloat, ())
            projectedLines =
                (\p -> (modelViewProj !* p, ())) .
                (\p -> point (p + offset)) .
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

-- Too slow?
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
                color' = applyFog fog color (fogDistance * 0.04)

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

{-
Calculer la AABB pour le frustum complet.
Identifier les blocs 4x4x4 de cette AABB en intersection avec le frustum.
Subdiviser les blocs (2x2x2) aussi en intersection avec le frustum intermédiaire.
Subdiviser encore les blocs (1x1x1) en intersection avec le frustum proche.

Afficher les blocs dans une vue de dieu.
-}
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

generateBlocks :: Window os RGBAFloat Depth -> ContextT GLFW.Handle os IO [Buffer os (B3 Float, B3 Float)]
generateBlocks window = do
    let blockBufferSize = blockSize^3 * maxCellTriangleCount * 3

        -- offsets = [ fromIntegral . (* blockSize) <$> V3 x y z | x <- [0], y <- [0], z <- [0] ]
        offsets = [ fromIntegral . (* blockSize) <$> V3 x y z | x <- [-3 .. 2], y <- [-3 .. 2], z <- [-1 .. 2] ]
        -- offsets = [ fromIntegral . (* blockSize) <$> V3 x y z | x <- [-5 .. 4], y <- [-5 .. 4], z <- [-3 .. 2] ]

    offsetBuffer :: Buffer os (Uniform (B3 Float)) <- newBuffer 1
    densityTexture :: Texture3D os (Format RFloat) <- newTexture3D R16F (pure (blockSize + 1)) 1
    noiseTextures :: [Texture3D os (Format RFloat)] <- take 6 . cycle <$> replicateM 3 (generate3DNoiseTexture (16, 16, 16))

    fillDensityTexture <- createFillDensityRenderer offsetBuffer densityTexture noiseTextures
    generateCell <- createGenerateBlockRenderer window offsetBuffer densityTexture

    let generateCellFromOffset offset blockBuffer = do
            writeBuffer offsetBuffer 0 [offset]
            render fillDensityTexture
            render (generateCell blockBuffer)

    blockBuffers :: [Buffer os (B3 Float, B3 Float)] <- replicateM (length offsets) (newBuffer blockBufferSize)

    -- Creating blocks.
    forM_ (zip offsets blockBuffers) $ \(offset, blockBuffer) -> do
        generateCellFromOffset offset blockBuffer

    return blockBuffers

-- glxinfo | egrep -i 'Currently available dedicated video memory'
createPolygonisationRenderer :: Window os RGBAFloat Depth -> ContextT GLFW.Handle os IO (RenderContext os)
createPolygonisationRenderer window = do
    blockBuffers :: [Buffer os (B3 Float, B3 Float)] <- generateBlocks window

    fogBuffer :: Buffer os (Uniform FogB) <- newBuffer 1
    sunBuffer :: Buffer os (Uniform DirectionLightB) <- newBuffer 1
    projectionBuffer :: Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float)) <- newBuffer 2
    offsetBuffer :: Buffer os (Uniform (B3 Float)) <- newBuffer 1
    outlineBuffer :: Buffer os (B Float, B3 Float) <- newBuffer 400

    blockRenderer <- createBlockRenderer window projectionBuffer fogBuffer sunBuffer
    blockOutlineRenderer <- createBlockOutlineRenderer window offsetBuffer projectionBuffer
    blockOutlinesRenderer <- createBlockOutlinesRenderer window projectionBuffer outlineBuffer
    gridRenderer <- createGridRenderer window projectionBuffer fogBuffer
    frustumRenderer <- createFrustumRenderer window projectionBuffer

    writeBuffer fogBuffer 0 [Fog (point skyBlue) 10 100 0.2]
    writeBuffer offsetBuffer 0 [pure 0]

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
                forM_ blockBuffers $ \blockBuffer -> blockRenderer (viewPort, blockBuffer)
                -- blockOutlineRenderer viewPort
                frustumRenderer viewPort
                blockOutlinesRenderer viewPort (length blocks)
                gridRenderer viewPort

            return $ RenderContext Nothing renderIt

    return (RenderContext Nothing renderIt)

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

    distanceToSight p = -(cameraMat !* point p)^._z

    size = blockSize * 4

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
        in  withSize s farestBlocks ++ divide (depth - 1) s' f' subBlocks

    allBlocks :: [(Float, V3 Float)]
    allBlocks = divide 2 size (cameraFar camera) blocks

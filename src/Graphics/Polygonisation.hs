{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies, FlexibleContexts #-}

module Graphics.Polygonisation
    ( createPolygonisationRenderer
    )
where

import Prelude hiding ((.), id, (<*))
import Control.Category (Category((.)), id)
import Control.Lens ((&), (<&>), (^.))
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Exception (MonadException)
import Data.Int (Int8, Int32)
import Data.Word (Word8)
import Data.List (intersect, partition)
import Data.Maybe (fromJust)

import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW

import Common.Debug
import Graphics.Shader
import Graphics.Geometry
import Graphics.Shader

----------------------------------------------------------------------------------------------------------------------

{- What I’m missing:
- Indexed (fixed size) array.
- Bitwise operations to pack/unpack data efficiently.
-}

----------------------------------------------------------------------------------------------------------------------

createFillDensityRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) =>
    Buffer os (Uniform (B3 Float)) ->
    Texture3D os (Format RFloat) ->
    ContextT ctx os m (Render os ())
createFillDensityRenderer offsetBuffer densityTexture = do
    let (V3 size _ _) = head (texture3DSizes densityTexture)

    planeBuffer :: Buffer os (B2 Float) <- newBuffer 6
    writeBuffer planeBuffer 0 [V2 (-1) (-1), V2 1 (-1), V2 1 1, V2 1 1, V2 (-1) 1, V2 (-1) (-1)]

    heightBuffer :: Buffer os (B Float) <- newBuffer size
    writeBuffer heightBuffer 0 (fromIntegral <$> [0 .. size - 1])

    shader :: CompiledShader os (Image (Format RFloat), PrimitiveArray Triangles (B Float, B2 Float))  <- compileShader $ do

        V3 dx dy dz <- getUniform (const (offsetBuffer, 0))

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

        {-
        let toV3 x y z = 1 / (1 + abs (x*x + y*y + z*z - 1000) / 20)
            fs' = (\(V3 x y z) -> toV3 (x + dx - 10) (y + dy + 3) (z + dz - 10)) <$> fs
        -}

        let toV3 x y z = x*x + y*y + z*z - 1000
            fs' = (\(V3 x y z) -> toV3 (x + dx - 10) (y + dy + 3) (z + dz - 10)) <$> fs

        -- let fs' = (\(V3 x y z) -> minB 1 . maxB 0 $ x - 5) <$> fs -- Leads to a crash.
        -- let fs' = (\(V3 x y z) -> minB 1 . maxB 0 $ (x*0) + (y*0) + (-z+dz) + 10) <$> fs

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
        cellCount = ((size - 1) ^ (3 :: Int))

    cellPositionBuffer :: Buffer os (B3 Int8) <- newBuffer cellCount
    writeBuffer cellPositionBuffer 0 [ fromIntegral <$> V3 x y z | x <- [0 .. size-2], y <- [0 .. size-2], z <- [0 .. size-2] ]

    let protoTriangleLists = map generateCaseProtoTriangleList (generateBoolCase 8)
        toCaseContent :: [(EdgeIndice, EdgeIndice, EdgeIndice)] -> [V3 Int8] -- We only need a [0, 1] domain actually…
        toCaseContent protoTriangleList = concatMap (\(e1, e2, e3) -> concatMap ((\(i1, i2) -> map (int8Cube !!) [i1, i2]). (cubeEdges !!)) [e1, e2, e3]) protoTriangleList
        toPaddedCaseContent protoTriangleList = take (4 * 3 * 2) (toCaseContent protoTriangleList ++ repeat (V3 0 0 0))

    arityTexture :: Texture1D os (Format RInt) <- newTexture1D R8I 256 1
    writeTexture1D arityTexture 0 0 256 ((map (fromIntegral . length) protoTriangleLists) :: [Int32])

    casesTexture :: Texture2D os (Format RGBInt) <- newTexture2D RGB8I (V2 24 256) 1
    writeTexture2D casesTexture 0 0 (V2 24 256) (concatMap toPaddedCaseContent protoTriangleLists)

    shader :: CompiledShader os (Buffer os (B3 Float, B3 Float), PrimitiveArray Points (B3 Int8))  <- compileShader $ do

        offset <- getUniform (const (offsetBuffer, 0))

        densitySampler <- newSampler3D (const (densityTexture, SamplerNearest, (pure ClampToEdge, undefined)))
        aritySampler <- newSampler1D (const (arityTexture, SamplerNearest, (ClampToEdge, undefined)))
        caseSampler <- newSampler2D (const (casesTexture, SamplerNearest, (pure ClampToEdge, undefined)))

        ps :: primitiveStream Points (V3 VInt) <- toPrimitiveStream snd

        gs :: GeometryStream (Geometry Points (V3 VInt)) <- geometrize ps
        let makeTriangles :: Geometry Points (V3 VInt) -> GGenerativeGeometry Triangles (V3 VFloat, V3 VFloat)
            makeTriangles (Point p) = triangles
                where
                    threshold = 0 - 0.5

                    getDensity :: V3 VInt -> VFloat
                    getDensity = texelFetch3D densitySampler (pure 0) 0

                    densities :: [VFloat]
                    densities = map getDensity $ fmap (p +) vintCube

                    getArity :: VInt -> VInt
                    getArity = texelFetch1D aritySampler (pure 0) 0

                    getCase :: V2 VInt -> V3 VInt
                    getCase = (texelFetch2D caseSampler (pure 0) 0)

                    -- 2^(7-i) benefits of being statically evaluated.
                    cellCase :: VInt
                    cellCase = foldl1 or' (zipWith (\i d -> ifB (d >* threshold) (fromIntegral ((2^(7-i))::Int)) 0) [0..] densities)

                    count = getArity cellCase

                    triangles :: GGenerativeGeometry Triangles (V3 VFloat, V3 VFloat)
                    (_, triangles) = while
                        (\(i, _) -> i <* count)
                        (\(i, gg) -> (i+1, (emitTriangle (i-1)) gg)) -- TODO Generated code seems to have a bug: i is modified with side effect.
                        (0, generativeTriangleStrip)

                    emitTriangle i =
                        let [i0, j0, i1, j1, i2, j2] = [ p + getCase (V2 (i * 6 + fromIntegral o) cellCase) | o <- [0..5] ]
                            calculatePoint :: V3 VInt -> V3 VInt -> V3 VFloat
                            calculatePoint v1 v2 = offset + (1 - a) *^ (toFloat <$> v1) + a *^ (toFloat <$> v2) where
                                (d1, d2) = (getDensity v1, getDensity v2)
                                a = minB 1 . maxB 0 $ (d1 - threshold) / (d1 - d2)
                                -- a = (d1 - threshold) / (d1 - d2)
                            p1 = calculatePoint i0 j0
                            p2 = calculatePoint i2 j2
                            p3 = calculatePoint i1 j1
                            n = signorm (cross (p3 - p1) (p3 - p2))
                        in  endPrimitive .
                            emitVertex (p1, n) .
                            emitVertex (p2, n) .
                            emitVertex (p3, n)

            gs' :: GeometryStream (GGenerativeGeometry Triangles (V3 VFloat, V3 VFloat)) = gs <&> makeTriangles

        let maxVertices = 4 * 3
        drawNothing window fst maxVertices gs'

    return $ \blockBuffer -> do
        positions <- toPrimitiveArray PointList <$> newVertexArray cellPositionBuffer
        shader (blockBuffer, positions)

createBlockRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) =>
    Window os RGBAFloat Depth ->
    Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float)) ->
    ContextT ctx os m ((V2 Int, Buffer os (B3 Float, B3 Float)) -> Render os ())
createBlockRenderer window projectionBuffer = do
    shader :: CompiledShader os (V2 Int, (Buffer os (B3 Float, B3 Float), PrimitiveArray Triangles (B3 Float, B3 Float)))  <- compileShader $ do

        (projectionMat, cameraMat, _) <- getUniform (const (projectionBuffer, 0))
        let modelViewProj = projectionMat !*! cameraMat

        ps :: primitiveStream Triangles (V3 VFloat, V3 VFloat) <- toPrimitiveStream' (Just (fst . snd)) (snd . snd)
        let ps' :: primitiveStream Triangles (VPos, V3 VFloat) = ps <&> \(p, n) ->
                let p' = modelViewProj !* (point p)
                    n' = modelViewProj !* vector n
                in  (p', v4To3 n')

        let rasterOptions = \(size, _) -> (FrontAndBack, ViewPort 0 size, DepthRange 0 1)
        fs :: FragmentStream (V4 FFloat, FragDepth) <- rasterize rasterOptions ps' <&> withRasterizedInfo (\n p ->
                let c = getSunlight n (V4 0.5 0.8 0.2 1)
                in  (c, rasterizedFragCoord p ^. _z))

        let colorOption = ContextColorOption NoBlending (pure True)
            depthOption = DepthOption Less True
        drawWindowColorDepth (const (window, colorOption, depthOption)) fs

    return $ \(size, blockBuffer) -> do
        block <- toPrimitiveArray TriangleList <$> newVertexArray blockBuffer
        shader (size, (blockBuffer, block))

createBlockOutlineRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) =>
    Window os RGBAFloat Depth ->
    Buffer os (Uniform (B3 Float)) ->
    Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float)) ->
    ContextT ctx os m (V2 Int -> Render os ())
createBlockOutlineRenderer window offsetBuffer projectionBuffer = do

    blockOutlineBuffer :: Buffer os (B3 Float) <- newBuffer (length floatCube)
    writeBuffer blockOutlineBuffer 0 floatCube

    let edges = concatMap (\(i, j) -> [i, j]) $ filter (\(i, j) -> j > i) cubeEdges
    blockOutlineIndexBuffer :: Buffer os (BPacked Word8) <- newBuffer (length edges)
    writeBuffer blockOutlineIndexBuffer 0 (map fromIntegral edges)

    shader :: CompiledShader os (V2 Int, PrimitiveArray Lines (B3 Float))  <- compileShader $ do
        (projectionMat, cameraMat, _) <- getUniform (const (projectionBuffer, 0))
        let modelViewProj = projectionMat !*! cameraMat

        offset <- getUniform (const (offsetBuffer, 0))

        lines :: PrimitiveStream Lines (V3 VFloat) <- toPrimitiveStream snd
        let
            projectedLines :: PrimitiveStream Lines (V4 VFloat, ())
            projectedLines =
                (\p -> (modelViewProj !* p, ())) .
                (\p -> point (p * 32 + offset)) <$>
                lines

        let rasterOptions = \(size, _) -> (Front, ViewPort 0 size, DepthRange 0 1)
        fs :: FragmentStream (V4 FFloat, FragDepth) <- rasterize rasterOptions projectedLines
            <&> withRasterizedInfo (\_ p -> (pure 1, rasterizedFragCoord p ^. _z - 0.0000001))

        let colorOption = ContextColorOption NoBlending (pure True)
            depthOption = DepthOption Less True
        drawWindowColorDepth (const (window, colorOption, depthOption)) fs

    return $ \size -> do
        blockOutline <- toPrimitiveArrayIndexed LineList
            <$> newIndexArray blockOutlineIndexBuffer Nothing
            <*> newVertexArray blockOutlineBuffer
        shader (size, blockOutline)

getSunlight :: V3 FFloat -> V4 FFloat -> V4 FFloat
getSunlight normal material =
    let sunLightColor = V3 0.5 0.5 0.5
        sunLightAmbientIntensity = 0.8
        sunLightDirection = - V3 1 1 0
        baseColor = material^._xyz

        ambient = sunLightColor ^* sunLightAmbientIntensity
        diffuse = sunLightColor ^* maxB 0 (dot normal (-sunLightDirection))
        sunContribution = baseColor * ambient + baseColor * diffuse

        color = v3To4 sunContribution 1

    in  ifThenElse' (material^._w <* 1) (V4 0.5 0.5 0.5 1) color

v3To4 :: Floating a => V3 a -> a -> V4 a
v3To4 (V3 x y z) w = V4 x y z w

v4To3 :: Floating a => V4 a -> V3 a
v4To3 (V4 x y z _) = V3 x y z

v4To3' :: Floating a => V4 a -> V3 a
v4To3' (V4 x y z w) = V3 (x/w) (y/w) (z/w)

createGridRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) =>
    Window os RGBAFloat Depth ->
    Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float)) ->
    ContextT ctx os m (V2 Int -> Render os ())
createGridRenderer window projectionBuffer = do
    let grid =
            [ V3 1 1 0,  V3 (-1) 1 0,  V3 1 (-1) 0
            , V3 (-1) (-1) 0,  V3 1 (-1) 0,  V3 (-1) 1 0
            ]

    gridBuffer :: Buffer os (B3 Float) <- newBuffer (length grid)
    writeBuffer gridBuffer 0 grid

    shader :: CompiledShader os (V2 Int, PrimitiveArray Triangles (B3 Float))  <- compileShader $ do
        (projectionMat, cameraMat, camPos) <- getUniform (const (projectionBuffer, 0))
        let modelViewProj = projectionMat !*! cameraMat

        triangles :: PrimitiveStream Triangles (V3 VFloat) <- toPrimitiveStream snd
        let
            projectedTriangles :: PrimitiveStream Triangles (V4 VFloat, (V2 VFloat, VFloat))
            projectedTriangles =
                (\p -> (modelViewProj !* p, (p^._xy, camPos^._z))) .
                (\p -> v3To4 p 1).
                (* 4000) <$> triangles

            getRasterOptions (size, _) = (FrontAndBack, ViewPort 0 size, DepthRange 0 1)

        fragCoord :: FragmentStream (V2 FFloat, FFloat) <- rasterize getRasterOptions projectedTriangles
        let
            drawGridLine :: (V2 FFloat, FFloat) -> V4 FFloat
            drawGridLine (p@(V2 x y), camPosZ) = color' where
                fog = FogS (V4 0.5 0.5 0.5 1) 10 100 0.2
                -- fog = FogS (V4 0 0 0 1) 10 100 0.4
                -- Pick a coordinate to visualize in a grid.
                (coord, coord') = (p / 32, p / 320)
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
                color = V4 r g b (1 - minB (line) 1)
                -- Add fog.
                fogDistance = norm $ V3 (p^._x) (p^._y) camPosZ
                color' = applyFog fog color (fogDistance * 0.01)

            litFrags = drawGridLine <$> fragCoord

            litFragsWithDepth = withRasterizedInfo
                (\a p -> (a, rasterizedFragCoord p ^._z)) litFrags
            blending = BlendRgbAlpha
                (FuncAdd, FuncAdd)
                (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors Zero Zero)
                (pure 1)
            colorOption = ContextColorOption blending (pure True)
            depthOption = DepthOption Less True

        drawWindowColorDepth (const (window, colorOption, depthOption)) litFragsWithDepth

    return $ \size -> do
        gridPrimArray <- toPrimitiveArray TriangleList <$> newVertexArray gridBuffer
        shader (size, gridPrimArray)

createPolygonisationRenderer :: Window os RGBAFloat Depth -> ContextT GLFW.Handle os IO (RenderContext os)
createPolygonisationRenderer window = do
    let blockSize = 32

    projectionBuffer :: Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float)) <- newBuffer 1
    offsetBuffer :: Buffer os (Uniform (B3 Float)) <- newBuffer 1
    densityTexture :: Texture3D os (Format RFloat) <- newTexture3D R16F (pure (blockSize + 1)) 1
    let blockBufferSize = blockSize^3 * 4 * 3 * 2 `div` 60

    fillDensityTexture <- createFillDensityRenderer offsetBuffer densityTexture
    generateCell <- createGenerateBlockRenderer window offsetBuffer densityTexture
    blockRenderer <- createBlockRenderer window projectionBuffer
    blockOutlineRenderer <- createBlockOutlineRenderer window offsetBuffer projectionBuffer
    gridRenderer <- createGridRenderer window projectionBuffer

    let generateCellFromOffset offset blockBuffer = do
            writeBuffer offsetBuffer 0 [offset]
            render fillDensityTexture
            -- OffsetBuffer must be rewritten to make things work… Why? Should
            -- everything be provide as a parameter instead as a lexical closure
            -- (at least regarding uniform)?
            writeBuffer offsetBuffer 0 [offset]
            render (generateCell blockBuffer)

    let offsets = [ fromIntegral . (* blockSize) <$> V3 x y z | x <- [-2 .. 1], y <- [-2 .. 1], z <- [-1 .. 1] ]
    -- let offsets = [ fromIntegral . (* blockSize) <$> V3 x y z | x <- [0], y <- [0], z <- [0] ]
    blockBuffers :: [Buffer os (B3 Float, B3 Float)] <- replicateM (length offsets) (newBuffer blockBufferSize)

    forM_ (zip offsets blockBuffers) $ \(offset, blockBuffer) -> do
        generateCellFromOffset offset blockBuffer

    let renderIt :: Int
            -> RenderContext os
            -> ((Int, Int), (Int, Int))
            -> Camera
            -> DirectionLight
            -> [PointLight]
            -> [Buffer os (B3 Float, B3 Float)]
            -> [Buffer os (B3 Float)]
            -> ContextT GLFW.Handle os IO (RenderContext os)
        renderIt i _ bounds camera _ _ _ _ = do
            let (_ , (w, h)) = bounds
                -- FOV (y direction, in radians), Aspect ratio, Near plane, Far plane
                projectionMat = perspective (cameraFov camera) (fromIntegral w / fromIntegral h) near far
                -- Eye, Center, Up
                cameraMat = lookAt
                    (cameraPos)
                    (cameraPos + getSight camera)
                    (getUp camera)
                cameraPos = cameraPosition camera

            writeBuffer projectionBuffer 0 [(projectionMat, cameraMat, cameraPos)]
            render $ do
                -- clearWindowColor window 0
                clearWindowDepth window 1
                forM_ blockBuffers $ \blockBuffer -> blockRenderer (V2 w h, blockBuffer)

            -- TODO Instanced rendering?
            forM_ offsets $ \offset -> do
                writeBuffer offsetBuffer 0 [offset]
                render $ do
                    blockOutlineRenderer (V2 w h)

            render $ gridRenderer (V2 w h)

            return $ RenderContext Nothing (renderIt (i+1))

    return (RenderContext Nothing (renderIt 0))

findConvexSubGraphs :: (a -> [a] -> Bool) -> [a] -> [[a]]
findConvexSubGraphs _ [] = []
findConvexSubGraphs isConnected (x:xs) = let
    classes = partition (isConnected x) (findConvexSubGraphs isConnected xs)
    in case classes of
        ([], ocs) -> [x]:ocs
        (cs, ocs) -> (x:(concat cs)):ocs

type VerticeIndice = Int
type EdgeIndice = Int
type FaceIndice = Int

floatCube :: [V3 Float]
floatCube =
    [ V3 0 0 0
    , V3 1 0 0
    , V3 0 1 0
    , V3 1 1 0
    , V3 0 0 1
    , V3 1 0 1
    , V3 0 1 1
    , V3 1 1 1
    ]

vintCube :: [V3 VInt]
vintCube =
    [ V3 0 0 0
    , V3 1 0 0
    , V3 0 1 0
    , V3 1 1 0
    , V3 0 0 1
    , V3 1 0 1
    , V3 0 1 1
    , V3 1 1 1
    ]

int8Cube :: [V3 Int8]
int8Cube =
    [ V3 0 0 0
    , V3 1 0 0
    , V3 0 1 0
    , V3 1 1 0
    , V3 0 0 1
    , V3 1 0 1
    , V3 0 1 1
    , V3 1 1 1
    ]

-- directed edges actually
cubeEdges :: [(VerticeIndice, VerticeIndice)] =
    [ (0, 1), (0, 2), (0, 4) -- <- 0
    , (1, 0), (1, 3), (1, 5) -- <- 3
    , (2, 0), (2, 3), (2, 6) -- <- 6
    , (3, 1), (3, 2), (3, 7) -- <- 9
    , (4, 0), (4, 5), (4, 6) -- <- 12
    , (5, 1), (5, 4), (5, 7) -- <- 15
    , (6, 2), (6, 4), (6, 7) -- <- 18
    , (7, 3), (7, 5), (7, 6) -- <- 21
    ]

-- case -> list of proto triangles of edge indices
generateCaseProtoTriangleList :: [Bool] -> [(EdgeIndice, EdgeIndice, EdgeIndice)]
generateCaseProtoTriangleList aboveness | length (_traceList "aboveness" aboveness) == 8 = triangles where
    {-
         4+----+5
         /|   /|
       6+-|--+7|
        |0+--|-|1
        |/   |/
       2+----+3
    -}
    vertices :: [VerticeIndice] = [0 .. 7]

    edges = cubeEdges

    {-
             5 4
             |/
         3---+---1
            /|
           2 0
    -}
    faces :: [FaceIndice] = [0 .. 5]

    folds :: [({- left -} FaceIndice, {- right -} FaceIndice)] -- !! EdgeIndice -- when looking inside the cube oriented along the edge
    folds =
        [ (0, 4), (3, 0), (4, 3) -- <- 0
        , (4, 0), (0, 1), (1, 4) -- <- 3
        , (0, 3), (2, 0), (3, 2) -- <- 6
        , (1, 0), (0, 2), (2, 1) -- <- 9
        , (3, 4), (4, 5), (5, 3) -- <- 12
        , (4, 1), (5, 4), (1, 5) -- <- 15
        , (2, 3), (3, 5), (5, 2) -- <- 18
        , (1, 2), (5, 1), (2, 5) -- <- 21
        ]
    getLeft = fst . (folds !!)
    getRight = snd . (folds !!)

    (innerEdges, transitionEdges) = partition (\(i1, i2) -> aboveness !! i1 == aboveness !! i2) edges

    -- findConvexSubGraphs the vertices in connected graphs where all vertices are above (the isles) or below (the seas).
    isInnerConnected i =
        let neighbours = map snd (filter ((==) i . fst) (_traceList "innerEdges" innerEdges))
        in  not . null . intersect neighbours
    (isles, seas) = partition ((aboveness !!) . head) (findConvexSubGraphs isInnerConnected vertices)

    findShoreEdges area = filter ((`elem` area) . fst) (_traceList "transitionEdges" transitionEdges)
    isleShoreEdges = map findShoreEdges (_traceList "isles" isles)
    seaShoreEdges = map findShoreEdges (_traceList "seas" seas)

    {-
    When an isle has shores leading to many seas, normalizing them could be complicated.
    Even if it is not the case, the result could be different when isles and seas are
    reversed. seems easier to select the more fragmented part, isles or sea, and change
    the edge directions for the latter.
    -}
    shoreEdges = if (length isles > length seas)
        then isleShoreEdges
        else map (map (\(i, j) -> (j, i))) seaShoreEdges

    shores :: [[EdgeIndice]]
    shores =
        let edgeIndex = zipWith (flip (,)) [0 ..] edges
        in  map (map (\edge -> fromJust (lookup edge edgeIndex))) shoreEdges

    normalizeShore :: Maybe FaceIndice -> [EdgeIndice] -> [EdgeIndice]
    normalizeShore _ [] = []
    normalizeShore Nothing (e:es) = e : normalizeShore (Just (getRight e)) es
    normalizeShore (Just expectedLeftFaceIndice) es =
        let ([edgeToTheRight], otherEdges) = partition ((expectedLeftFaceIndice ==) . getLeft) es
        in  edgeToTheRight : normalizeShore (Just (getRight edgeToTheRight)) otherEdges
    normalizedShores = map (normalizeShore Nothing) (_traceList "shores" shores)

    triangles =
        let ts = _traceList "triangles" $ concatMap toTriangleList (_traceList "normalizedShores" normalizedShores)
        in  if length ts <= 12 then ts else error "too many vertices generated"

    toTriangleList ps
        | length ps <= 7 = toTriangleList' ps
        | otherwise = error $ "Too many edges: " ++ show (length ps)

    -- TODO Generate triangle strips instead.
    toTriangleList' :: [a] -> [(a, a, a)]
    toTriangleList' [] = []
    toTriangleList' (p:ps) = zipWith (\pN pM -> (p, pN, pM)) ps (tail ps)

generateCaseProtoTriangleList _ | otherwise = error "aboveness must contains data for 8 vertices"

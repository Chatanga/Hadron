{-# language ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}

module Graphics.CubeRoom
    ( createCubeRoomRenderer
    , createCubePreRenderer
    )
where

import Prelude hiding ((.), id, (<*))
import Control.Category (Category((.)), id)
import Control.Lens ((&), (<&>), (^.))
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Exception
import Data.Bifunctor ( Bifunctor(first) )
import Data.Int (Int8, Int32)
import Data.Word (Word8, Word16, Word32)
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import Numeric

import Common.Debug
import Graphics.Color
import Graphics.Font
import Graphics.Geometry
import Graphics.MarchingCube
import Graphics.Shaders
import Graphics.World

----------------------------------------------------------------------------------------------------------------------

generateCase :: Int -> [Int]
generateCase 0 = [0]
generateCase i = [ x + xs | x <- [0, 2^(i - 1)], xs <- generateCase (i - 1) ]

generateBoolCase :: Int -> [[Bool]]
generateBoolCase 0 = [[]]
generateBoolCase i = [ x:xs | x <- [False, True], xs <- generateBoolCase (i - 1) ]

verticeFromEdge :: Int -> V3 Float
verticeFromEdge egdeIndice = let (v1, v2) = cubeEdges!!egdeIndice in fmap fromIntegral (cube!!v1 + cube!!v2) / 2

instanciateProtoTriangleList :: Bool -> [(EdgeIndice, EdgeIndice, EdgeIndice)] -> [(V3 Float, V3 Float)]
instanciateProtoTriangleList flipped = concatMap f where
    f (e1, e2, e3) =
        let [v1, v2, v3] = if flipped
            then [verticeFromEdge e1, verticeFromEdge e3, verticeFromEdge e2]
            else [verticeFromEdge e1, verticeFromEdge e2, verticeFromEdge e3]
            n = normalize ((v2 - v1) `cross` (v3 - v1))
        in  [(v1, n), (v2, n), (v3, n)]

weightedVerticeFromEdge :: [Float] -> Int -> V3 Float
weightedVerticeFromEdge densities egdeIndice =
    let (v1, v2) = cubeEdges!!egdeIndice
        (d1, d2) = (densities!!v1, densities!!v2)
        a = d1 / (d1 - d2)
    in  (1 - a) *^ (cube!!v1) + a *^ (cube!!v2)

instanciateTriangleList :: Bool -> [Float] -> [(EdgeIndice, EdgeIndice, EdgeIndice)] -> [(V3 Float, V3 Float)]
instanciateTriangleList flipped densities = concatMap f where
    f (e1, e2, e3) =
        let getVertice = weightedVerticeFromEdge densities
            [v1, v2, v3] = if flipped
            then [getVertice e1, getVertice e3, getVertice e2]
            else [getVertice e1, getVertice e2, getVertice e3]
            n = normalize ((v2 - v1) `cross` (v3 - v1))
        in  [(v1, n), (v2, n), (v3, n)]

createBlocksRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) =>
    Window os RGBAFloat Depth ->
    Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float)) ->
    Buffer os (Uniform FogB) ->
    Buffer os (Uniform DirectionLightB) ->
    Buffer os (B3 Float, B3 Float, B Int32) ->
    ContextT ctx os m (ViewPort -> Int -> Render os ())
createBlocksRenderer window projectionBuffer fogBuffer sunBuffer blockBuffer = do

    shader :: CompiledShader os (ViewPort, PrimitiveArray Triangles (B3 Float, B3 Float, B Int32))  <- compileShader $ do

        (projectionMat, cameraMat, cameraPos) <- getUniform (const (projectionBuffer, 0))
        let modelViewProj = projectionMat !*! cameraMat

        ps :: primitiveStream Triangles (V3 VFloat, V3 VFloat, VInt) <- toPrimitiveStream snd
        let ps' :: primitiveStream Triangles (VPos, (V3 VFloat, V3 VFloat, V3 VFloat, V4 VFloat)) = ps <&>
                \(p, n, above) ->
                    let m = ifThenElse' (above >* 0) (V4 1 1 1 1) (V4 0 0 0 1)
                    in  (modelViewProj !* point p, (p, n, cameraPos, m))

        fog :: FogS F <- getUniform (const (fogBuffer, 0))
        sun :: DirectionLightS F <- getUniform (const (sunBuffer, 0))

        let rasterOptions = \(viewPort, _) -> (Front, viewPort, DepthRange 0 1)
        fs :: FragmentStream (V4 FFloat, FragDepth) <- rasterize rasterOptions ps' <&> withRasterizedInfo (\(p, n, cp, m) ri ->
                let lightingContext =
                        ( cp
                        , fog
                        , sun{ directionLightColorS = V3 0.7 0.7 0.7, directionLightAmbientIntensityS = 0.8 }
                        , 0.6 -- material specular intensity
                        , 8 -- material specular power
                        )
                    c = getSunlight undefined n Nothing p m 1 lightingContext
                in  (c, rasterizedFragCoord ri ^. _z))

        let colorOption = ContextColorOption NoBlending (pure True)
            depthOption = DepthOption Lequal True
        drawWindowColorDepth (const (window, colorOption, depthOption)) fs

    return $ \viewPort primitiveCount -> do
        block <- toPrimitiveArray TriangleList . takeVertices primitiveCount <$> newVertexArray blockBuffer
        shader (viewPort, block)

createCubesRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) =>
    Window os RGBAFloat Depth ->
    Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float)) ->
    Buffer os (Uniform (B Float)) ->
    Buffer os (B3 Float) ->
    ContextT ctx os m (ViewPort -> Render os ())
createCubesRenderer window projectionBuffer scaleBuffer offsetBuffer = do

    blockOutlineBuffer :: Buffer os (B3 Float) <- newBuffer (length cube)
    writeBuffer blockOutlineBuffer 0 cube

    let edges = concatMap (\(i, j) -> [i, j]) $ filter (\(i, j) -> j > i) cubeEdges
    blockOutlineIndexBuffer :: Buffer os (BPacked Word8) <- newBuffer (length edges)
    writeBuffer blockOutlineIndexBuffer 0 (map fromIntegral edges)

    shader :: CompiledShader os (ViewPort, PrimitiveArray Lines (B3 Float, B3 Float))  <- compileShader $ do
        scale <- getUniform (const (scaleBuffer, 0))
        (projectionMat, cameraMat, _) <- getUniform (const (projectionBuffer, 0))
        let modelViewProj = projectionMat !*! cameraMat

        lines :: PrimitiveStream Lines (V3 VFloat, V3 VFloat) <- toPrimitiveStream snd
        let
            projectedLines :: PrimitiveStream Lines (V4 VFloat, ())
            projectedLines =
                (\p -> (modelViewProj !* p, ())) .
                (\(p, o) -> point (p ^* scale) + vector o) <$>
                lines

        let rasterOptions = \(viewPort, _) -> (Front, viewPort, DepthRange 0 1)
        fs :: FragmentStream (V4 FFloat, FragDepth) <- rasterize rasterOptions projectedLines
            <&> withRasterizedInfo (\_ p -> (pure 1, rasterizedFragCoord p ^. _z - 0.0000001))

        let colorOption = ContextColorOption NoBlending (pure True)
            depthOption = DepthOption Less True
        drawWindowColorDepth (const (window, colorOption, depthOption)) fs

    return $ \viewport -> do
        blockOutline <- toPrimitiveArrayIndexedInstanced LineList
            <$> newIndexArray blockOutlineIndexBuffer Nothing
            <*> return (,)
            <*> newVertexArray blockOutlineBuffer
            <*> newVertexArray offsetBuffer
        shader (viewport, blockOutline)

createCornersRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) =>
    Window os RGBAFloat Depth ->
    Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float)) ->
    Buffer os (Uniform FogB) ->
    Buffer os (Uniform DirectionLightB) ->
    Buffer os (B3 Float, B Int32) ->
    ContextT ctx os m (ViewPort -> Render os ())
createCornersRenderer window projectionBuffer fogBuffer sunBuffer cornerInstanceBuffer = do

    let corner = fmap (first (/ 20)) (sphere 3)

    cornerBuffer :: Buffer os (B3 Float, B3 Float) <- newBuffer (length corner)
    writeBuffer cornerBuffer 0 corner

    shader :: CompiledShader os (ViewPort, PrimitiveArray Triangles ((B3 Float, B3 Float), (B3 Float, B Int32)))  <- compileShader $ do

        (projectionMat, cameraMat, cameraPos) <- getUniform (const (projectionBuffer, 0))
        let modelViewProj = projectionMat !*! cameraMat

        ps :: primitiveStream Triangles ((V3 VFloat, V3 VFloat), (V3 VFloat, VInt)) <- toPrimitiveStream snd
        let ps' :: primitiveStream Triangles (VPos, (V3 VFloat, V3 VFloat, V3 VFloat, V4 VFloat)) = ps <&>
                \((p, n), (o, above)) ->
                    let p' = p + o
                        m = ifThenElse' (above >* 0) (V4 1 1 1 1) (V4 0 0 0 1)
                    in  (modelViewProj !* point p', (p', n, cameraPos, m))

        fog :: FogS F <- getUniform (const (fogBuffer, 0))
        sun :: DirectionLightS F <- getUniform (const (sunBuffer, 0))

        let rasterOptions = \(viewPort, _) -> (Front, viewPort, DepthRange 0 1)
        fs :: FragmentStream (V4 FFloat, FragDepth) <- rasterize rasterOptions ps' <&> withRasterizedInfo (\(p, n, cp, m) ri ->
                let lightingContext =
                        ( cp
                        , fog
                        , sun{ directionLightColorS = V3 0.7 0.7 0.7, directionLightAmbientIntensityS = 0.8 }
                        , 0.6 -- material specular intensity
                        , 8 -- material specular power
                        )
                    c = getSunlight undefined n Nothing p m 1 lightingContext
                in  (c, rasterizedFragCoord ri ^. _z))

        let colorOption = ContextColorOption NoBlending (pure True)
            depthOption = DepthOption Less True
        drawWindowColorDepth (const (window, colorOption, depthOption)) fs

    return $ \viewPort -> do
        corner <- toPrimitiveArrayInstanced TriangleList (,)
            <$> newVertexArray cornerBuffer
            <*> newVertexArray cornerInstanceBuffer
        shader (viewPort, corner)

createCubeRoomRenderer :: (MonadIO m, MonadAsyncException m) => Window os RGBAFloat Depth -> ContextT GLFW.Handle os m (RenderContext m os)
createCubeRoomRenderer window = do

    scaleBuffer :: Buffer os (Uniform (B Float)) <- newBuffer 1
    writeBuffer scaleBuffer 0 [1]

    fogBuffer :: Buffer os (Uniform FogB) <- newBuffer 1
    sunBuffer :: Buffer os (Uniform DirectionLightB) <- newBuffer 1
    projectionBuffer :: Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float)) <- newBuffer 1

    let allCases = concat $ replicate 16 $ generateBoolCase 8
        cubePosition i = fromIntegral <$> 2 * V3 ((i `mod` 256) `mod` 16) ((i `mod` 256) `div` 16) (i `div` 256)

        createCorners (i, aboveness) = zipWith (\p c -> (cubePosition i + p, if c then 1 else 0)) cube aboveness
        corners = concatMap createCorners (zip [0..] allCases)

        createBlock (i, aboveness) =
            let triangles = generateCaseProtoTriangleList aboveness
                vertices = instanciateProtoTriangleList False triangles
                backVertices = instanciateProtoTriangleList True triangles
                moveAndColorize c = map (\(v, n) -> (v + cubePosition i, n, c))
            in  moveAndColorize 1 vertices ++ moveAndColorize 0 backVertices
        blocks = concatMap createBlock (zip [0..] allCases)

    cubeOffsetBuffer :: Buffer os (B3 Float) <- newBuffer (length allCases)
    writeBuffer cubeOffsetBuffer 0 $ map cubePosition [0 .. length allCases - 1]

    cornerInstanceBuffer :: Buffer os (B3 Float, B Int32) <- newBuffer (length corners)
    writeBuffer cornerInstanceBuffer 0 corners

    blockBuffer :: Buffer os (B3 Float, B3 Float, B Int32) <- newBuffer (length blocks)
    writeBuffer blockBuffer 0 blocks

    cubesRenderer <- createCubesRenderer window projectionBuffer scaleBuffer cubeOffsetBuffer
    cornersRenderer <- createCornersRenderer window projectionBuffer fogBuffer sunBuffer cornerInstanceBuffer
    blocksRenderer <- createBlocksRenderer window projectionBuffer fogBuffer sunBuffer blockBuffer

    writeBuffer fogBuffer 0 [Fog (point skyBlue) 10 100 0.02]

    let renderIt _ bounds camera _ sun lights buffers normalBuffers _ _ = do

            writeBuffer sunBuffer 0 [sun]

            let ((x, y) , (w, h)) = bounds
            writeBuffer projectionBuffer 0 [createProjection bounds camera]

            render $ do
                clearWindowColor window (point skyBlue)
                clearWindowDepth window 1
                let viewPort = ViewPort (V2 x y) (V2 w h)
                cubesRenderer viewPort
                cornersRenderer viewPort
                blocksRenderer viewPort (length blocks)

            return $ RenderContext Nothing renderIt

    return (RenderContext Nothing renderIt)

-- ***

createCubePreRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m)
    => Window os RGBAFloat Depth
    -> Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float))
    -> Buffer os (Uniform FogB)
    -> Buffer os (Uniform DirectionLightB)
    -> ContextT ctx os m (((Float, V3 Float), ([Bool], [Float])) -> ContextT ctx os m (ViewPort -> Render os ()))
createCubePreRenderer window projectionBuffer fogBuffer sunBuffer = do

    scaleBuffer :: Buffer os (Uniform (B Float)) <- newBuffer 1
    cubeOffsetBuffer :: Buffer os (B3 Float) <- newBuffer 1
    cornerInstanceBuffer :: Buffer os (B3 Float, B Int32) <- newBuffer 8
    blockBuffer :: Buffer os (B3 Float, B3 Float, B Int32) <- newBuffer (5 * 3)

    cubesRenderer <- createCubesRenderer window projectionBuffer scaleBuffer cubeOffsetBuffer
    cornersRenderer <- createCornersRenderer window projectionBuffer fogBuffer sunBuffer cornerInstanceBuffer
    blocksRenderer <- createBlocksRenderer window projectionBuffer fogBuffer sunBuffer blockBuffer

    textPreRenderers <- replicateM 8 (createBitmapTextPreRenderer window projectionBuffer)

    return $ \((scale, offset), (aboveness, densities)) -> do

        let corners = zipWith (\p c -> (offset + p ^* scale, if c then 1 else 0)) cube aboveness

            blockTriangles =
                let triangles = generateCaseProtoTriangleList aboveness
                    vertices = instanciateTriangleList False densities triangles
                    backVertices = instanciateTriangleList True densities triangles
                    moveAndColorize c = map (\(v, n) -> (offset + v ^* scale, n, c))
                in  moveAndColorize 1 vertices ++ moveAndColorize 0 backVertices

        writeBuffer scaleBuffer 0 [scale]
        writeBuffer cubeOffsetBuffer 0 [offset]
        writeBuffer cornerInstanceBuffer 0 corners
        writeBuffer blockBuffer 0 blockTriangles

        textRenderers <- forM (zip3 textPreRenderers (map fst corners) densities) $ \(textPreRenderer, corner, density) -> do
            let text = showFFloat (Just 2) (density * 100) ""
            textPreRenderer corner 0.02 text

        return $ \viewPort -> do
            cubesRenderer viewPort
            cornersRenderer viewPort
            blocksRenderer viewPort (length blockTriangles)
            mapM_ ($ viewPort) textRenderers

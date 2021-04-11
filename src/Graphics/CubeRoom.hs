{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies, FlexibleContexts #-}

module Graphics.CubeRoom
    ( createCubeRoomRenderer
    )
where

import Prelude hiding ((.), id, (<*))
import Control.Category (Category((.)), id)
import Control.Lens ((&), (<&>), (^.))
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Exception (MonadException)
import Data.Bifunctor
import Data.Int (Int8, Int32)
import Data.Word (Word8, Word16, Word32)

import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW

import Common.Debug
import Graphics.Color
import Graphics.Geometry
import Graphics.MarchingCube
import Graphics.Shaders
import Graphics.World
import System.IO

----------------------------------------------------------------------------------------------------------------------

generateCase :: Int -> [Int]
generateCase 0 = [0]
generateCase i = [ x + xs | x <- [0, 2^(i - 1)], xs <- generateCase (i - 1) ]

generateBoolCase :: Int -> [[Bool]]
generateBoolCase 0 = [[]]
generateBoolCase i = [ x:xs | x <- [False, True], xs <- generateBoolCase (i - 1) ]

verticeFromEdge :: Int -> V3 Float
verticeFromEdge egdeIndice = let (v1, v2) = cubeEdges!!egdeIndice in fmap fromIntegral (cube!!v1 + cube!!v2) / 2

instanciateBackProtoTriangleList :: [(EdgeIndice, EdgeIndice, EdgeIndice)] -> [(V3 Float, V3 Float)]
instanciateBackProtoTriangleList = concatMap f where
    f (e1, e2, e3) =
        let [v1, v2, v3] = [verticeFromEdge e1, verticeFromEdge e2, verticeFromEdge e3]
            n = normalize ((v3 - v1) `cross` (v2 - v1))
        in  [(v1, n), (v3, n), (v2, n)]

instanciateProtoTriangleList :: [(EdgeIndice, EdgeIndice, EdgeIndice)] -> [(V3 Float, V3 Float)]
instanciateProtoTriangleList = concatMap f where
    f (e1, e2, e3) =
        let [v1, v2, v3] = [verticeFromEdge e1, verticeFromEdge e2, verticeFromEdge e3]
            n = normalize ((v2 - v1) `cross` (v3 - v1))
        in  [(v1, n), (v2, n), (v3, n)]

createSummits :: [Bool] -> Bool -> [(V3 Float, V3 Float)]
createSummits aboveness above =
    let offsets = map snd (filter ((==) above . fst) (zip aboveness cube))
    in  concatMap (\o -> map (first ((fmap fromIntegral o +) . (/ 20))) (sphere 3)) offsets

createBlockRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) =>
    Window os RGBAFloat Depth ->
    Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float)) ->
    Buffer os (Uniform FogB) ->
    Buffer os (Uniform DirectionLightB) ->
    Int ->
    ContextT ctx os m (ViewPort -> Render os ())
createBlockRenderer window projectionBuffer fogBuffer sunBuffer blockIndex = do

    let aboveness = generateBoolCase 8 !! blockIndex
        vertices = instanciateProtoTriangleList (generateCaseProtoTriangleList aboveness)
        backVertices = instanciateBackProtoTriangleList (generateCaseProtoTriangleList aboveness)
        aboveSummits = createSummits aboveness True
        belowSummits = createSummits aboveness False

        colorize c = map (\(v, n) -> (v, n, c))

    blockBuffer :: Buffer os (B3 Float, B3 Float, B4 Float) <- newBuffer (length vertices + length aboveSummits + length belowSummits)
    writeBuffer blockBuffer 0 $
        colorize (V4 1 1 1 1) vertices ++
        colorize (V4 0 0 0 1) backVertices ++
        colorize (V4 1 1 1 1) aboveSummits ++
        colorize (V4 0 0 0 1) belowSummits

    shader :: CompiledShader os (ViewPort, PrimitiveArray Triangles (B3 Float, B3 Float, B4 Float))  <- compileShader $ do

        (projectionMat, cameraMat, cameraPos) <- getUniform (const (projectionBuffer, 0))
        let modelViewProj = projectionMat !*! cameraMat

        ps :: primitiveStream Triangles (V3 VFloat, V3 VFloat, V4 VFloat) <- toPrimitiveStream snd
        let ps' :: primitiveStream Triangles (VPos, (V3 VFloat, V3 VFloat, V3 VFloat, V4 VFloat)) = ps <&> \(p, n, m) -> let p' = p + V3 (fromIntegral blockIndex * 2) 0 0 in (modelViewProj !* point p', (p', n, cameraPos, m))

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
        block <- toPrimitiveArray TriangleList <$> newVertexArray blockBuffer
        shader (viewPort, block)

createCubeRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) =>
    Window os RGBAFloat Depth ->
    Buffer os (Uniform (B3 Float)) ->
    Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float)) ->
    ContextT ctx os m (ViewPort -> Int -> Render os ())
createCubeRenderer window offsetBuffer projectionBuffer = do

    let floatCube :: [V3 Float]
        floatCube = map (fmap fromIntegral) cube

    blockOutlineBuffer :: Buffer os (B3 Float) <- newBuffer (length floatCube)
    writeBuffer blockOutlineBuffer 0 floatCube

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
                (\p -> point (p + offset)) <$>
                lines

        let rasterOptions = \(viewPort, _) -> (Front, viewPort, DepthRange 0 1)
        fs :: FragmentStream (V4 FFloat, FragDepth) <- rasterize rasterOptions projectedLines
            <&> withRasterizedInfo (\_ p -> (pure 1, rasterizedFragCoord p ^. _z - 0.0000001))

        let colorOption = ContextColorOption NoBlending (pure True)
            depthOption = DepthOption Less True
        drawWindowColorDepth (const (window, colorOption, depthOption)) fs

    return $ \viewport offsetIndex -> do
        blockOutline <- toPrimitiveArrayIndexed LineList
            <$> newIndexArray blockOutlineIndexBuffer Nothing
            <*> newVertexArray blockOutlineBuffer
        shader (viewport, (offsetIndex, blockOutline))

createCubeRoomRenderer :: Window os RGBAFloat Depth -> ContextT GLFW.Handle os IO (RenderContext os)
createCubeRoomRenderer window = do

    fogBuffer :: Buffer os (Uniform FogB) <- newBuffer 1
    sunBuffer :: Buffer os (Uniform DirectionLightB) <- newBuffer 1
    projectionBuffer :: Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float)) <- newBuffer 1

    offsetBuffer :: Buffer os (Uniform (B3 Float)) <- newBuffer 256
    writeBuffer offsetBuffer 0 $ take 256 $ iterate (+ V3 2 0 0) (V3 0 0 0)

    cubeRenderer <- createCubeRenderer window offsetBuffer projectionBuffer
    blockRenderers <- mapM (createBlockRenderer window projectionBuffer fogBuffer sunBuffer) [0..255]

    writeBuffer fogBuffer 0 [Fog (v3To4 skyBlue 1) 10 100 0.2]

    let renderIt :: RenderContext os
            -> ((Int, Int), (Int, Int))
            -> Camera
            -> DirectionLight
            -> [PointLight]
            -> [Buffer os (B3 Float, B3 Float)]
            -> [Buffer os (B3 Float)]
            -> ContextT GLFW.Handle os IO (RenderContext os)
        renderIt _ bounds camera sun lights buffers normalBuffers = do

            writeBuffer sunBuffer 0 [sun]

            let ((x, y) , (w, h)) = bounds
                -- FOV (y direction, in radians), Aspect ratio, Near plane, Far plane
                projectionMat = perspective (cameraFov camera) (fromIntegral w / fromIntegral h) near far
                -- Eye, Center, Up
                cameraMat = lookAt
                    cameraPos
                    (cameraPos + getSight camera)
                    (getUp camera)
                cameraPos = cameraPosition camera
            writeBuffer projectionBuffer 0 [(projectionMat, cameraMat, cameraPos)]

            render $ do
                clearWindowColor window (v3To4 skyBlue 1)
                clearWindowDepth window 1
                forM_ [0..255] $ \i -> do
                    let viewPort = ViewPort (V2 x y) (V2 w h)
                    cubeRenderer viewPort i
                    (blockRenderers !! i) viewPort

            return $ RenderContext Nothing renderIt

    return (RenderContext Nothing renderIt)

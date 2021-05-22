{-# language RankNTypes #-}

module Graphics.Scene
    ( Scene(..)
    , SceneName(..)
    , SceneContext(..)
    , createScene
    , createSceneContext
    ) where

import Control.Monad.State
import Control.Monad.Exception

import Data.Fixed
import Data.IORef
import Data.Maybe
import qualified Data.Map as Map
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import System.Log.Logger

import Common.Random
import Graphics.CubeRoom
import Graphics.Geometry
import Graphics.Incal
import Graphics.Polygonisation
import Graphics.Shaders
import Graphics.Texture
import Graphics.View
import Graphics.World

------------------------------------------------------------------------------------------------------------------------

data SceneContext m os = SceneContext
    { sceneContextCameraName :: String
    , sceneContextCameraMoves :: !(Map.Map Move Double)
    , sceneContextCursorPosition :: (Float, Float)
    , sceneContextDrag :: Maybe (Float, Float)
    , sceneContextRenderContext :: RenderContext m os
    }

data Move
    = GoUp
    | GoDown
    | GoLeft
    | GoRight
    | GoForth
    | GoBack
    deriving (Eq, Ord, Show)

data Scene m os = Scene
    { sceneDisplay :: MonadIO m => ((Int, Int), (Int, Int)) -> ContextT GLFW.Handle os m ()
    , sceneAnimate :: MonadIO m => (Float, Float) -> Double -> ContextT GLFW.Handle os m (Scene m os)
    , sceneManipulate :: MonadIO m => (Float, Float) -> Event -> ContextT GLFW.Handle os m (Maybe (Scene m os)) -- ^ nothing => exit
    }

createScene :: (MonadIO m, MonadAsyncException m) => Window os f Depth -> IORef (World os) -> IORef (SceneContext m os) -> Scene m os
createScene window worldRef contextRef = Scene
    (display window worldRef contextRef)
    (animate window worldRef contextRef)
    (manipulate window worldRef contextRef)

data SceneName = Incal | Polygonisation | CubeRoom

createSceneContext :: (MonadIO m, MonadAsyncException m) => Window os RGBAFloat Depth -> SceneName -> String -> ContextT GLFW.Handle os m (SceneContext m os)
createSceneContext window sceneName cameraName = do
    renderer <- case sceneName of
        Incal -> createIncalRenderer window
        Polygonisation -> createPolygonisationRenderer window
        CubeRoom -> createCubeRoomRenderer window
    return $ SceneContext
        cameraName
        Map.empty
        (0, 0)
        Nothing
        renderer

------------------------------------------------------------------------------------------------------------------------

display :: (MonadIO m, MonadAsyncException m)
    => Window os f Depth
    -> IORef (World os)
    -> IORef (SceneContext m os)
    -> ((Int, Int), (Int, Int))
    -> ContextT GLFW.Handle os m ()
display window worldRef contextRef bounds = do
    world <- liftIO $ readIORef worldRef
    context <- liftIO $ readIORef contextRef

    let camera = fromJust (lookup (sceneContextCameraName context) (worldCameras world))
        cameras = map snd (worldCameras world)
        sun = worldSun world
        lights = map (\(FireBall p _ c _) -> PointLight p c) (worldFireBalls world)
        buffers = worldBuffers world
        normalBuffers = worldNormalBuffers world
        renderContext = sceneContextRenderContext context

    newRenderContext <- renderContextRenderAction renderContext renderContext bounds camera cameras sun lights buffers normalBuffers

    liftIO $ writeIORef contextRef (context{ sceneContextRenderContext = newRenderContext })

------------------------------------------------------------------------------------------------------------------------

animate :: (MonadIO m, MonadAsyncException m)
    => Window os f Depth
    -> IORef (World os)
    -> IORef (SceneContext m os)
    -> (Float, Float)
    -> Double
    -> ContextT GLFW.Handle os m (Scene m os)
animate window worldRef contextRef (_, height) timeDelta = do
    world <- liftIO $ readIORef worldRef
    context <- liftIO $ readIORef contextRef

    -- Move a camera by applying any registered move for the given time delta.
    let moveCamera camera = camera
                { cameraAltitude = alt'
                , cameraAzimuth = az'
                , cameraPosition = position }
            where
                moves = sceneContextCameraMoves context
                keyMoves =
                    [ (GoUp, getUp camera)
                    , (GoDown, - (getUp camera))
                    , (GoLeft, getLeft camera)
                    , (GoRight, - (getLeft camera))
                    , (GoForth, getSight camera)
                    , (GoBack, - (getSight camera))
                    ]
                applyMove p (m, dp) = case Map.lookup m moves of
                    Just i -> p + dp * realToFrac (timeDelta * i)
                    Nothing -> p
                position = foldl applyMove (cameraPosition camera) keyMoves
                (alt, az) = (cameraAltitude camera, cameraAzimuth camera)
                (alt', az') = case sceneContextDrag context of
                    Nothing -> (alt, az)
                    Just (dx, dy) -> (alt', az') where
                        ratio = cameraFov camera / realToFrac height
                        (dAz, dAlt) = (dx * ratio, dy * ratio)
                        alt' = alt + clamp (-pi/2.01-alt) (pi/2.01-alt) dAlt
                        az' = Data.Fixed.mod' (az - dAz) (2 * pi)

    let for = flip map
        cameras = for (worldCameras world) $ \(n, c) ->
            if n == sceneContextCameraName context then (n, moveCamera c) else (n, c)

    liftIO $ writeIORef worldRef world{ worldCameras = cameras }
    liftIO $ writeIORef contextRef context{ sceneContextDrag = Nothing }
    return $ createScene window worldRef contextRef

------------------------------------------------------------------------------------------------------------------------

manipulate :: (MonadIO m, MonadAsyncException m)
    => Window os f Depth
    -> IORef (World os)
    -> IORef (SceneContext m os)
    -> (Float, Float)
    -> Event
    -> ContextT GLFW.Handle os m (Maybe (Scene m os))

-- Handle keyboard events.
manipulate window worldRef contextRef _ (EventKey k _ ks _)

    -- Exit application on Escape.
    | k == GLFW.Key'Escape = return Nothing

    -- Save intermediate buffer rendering into a file.
    | k == GLFW.Key'P && ks == GLFW.KeyState'Pressed = do
        context <- liftIO $ readIORef contextRef
        let Just (_, frameBufferGroup) = renderContextFrameBufferGroup (sceneContextRenderContext context)
            tex = frameBufferGroupOcclusionTex frameBufferGroup
            V2 w h = head (texture2DSizes tex)
        saveDepthTexture (w, h) tex "occlusion.png"
        liftIO $ infoM "Hadron" "Depth map saved"
        return $ Just (createScene window worldRef contextRef)

    -- Save intermediate buffer rendering into a file.
    | k == GLFW.Key'O && ks == GLFW.KeyState'Pressed = do
        context <- liftIO $ readIORef contextRef
        let Just (_, frameBufferGroup) = renderContextFrameBufferGroup (sceneContextRenderContext context)
            tex = frameBufferGroupPositionTex frameBufferGroup
            V2 w h = head (texture2DSizes tex)
        saveTexture (w, h) tex "position.png"
        liftIO $ infoM "Hadron" "Color buffer saved"
        return $ Just (createScene window worldRef contextRef)

    -- Next camera.
    | k == GLFW.Key'Tab && ks == GLFW.KeyState'Pressed = do
        world <- liftIO $ readIORef worldRef
        context <- liftIO $ readIORef contextRef
        let nextCameraName = dropWhile (/= sceneContextCameraName context) (cycle (map fst (worldCameras world))) !! 1
        liftIO $ writeIORef contextRef context{ sceneContextCameraName = nextCameraName }
        return $ Just (createScene window worldRef contextRef)

    -- Move the camera using WASD keys (note that the keyboard layout is not taken into account).
    | otherwise = do
        context <- liftIO $ readIORef contextRef
        let
            moves = sceneContextCameraMoves context
            handleKey = if ks == GLFW.KeyState'Released then Map.delete else flip (Map.insertWith (\i n -> i)) 50
            moves' = case k of
                GLFW.Key'Space -> handleKey GoUp moves
                GLFW.Key'LeftControl -> handleKey GoDown moves -- No auto-repeat on a modifier key.
                GLFW.Key'W -> handleKey GoForth moves
                GLFW.Key'S -> handleKey GoBack moves
                GLFW.Key'A -> handleKey GoLeft moves
                GLFW.Key'D -> handleKey GoRight moves
                _ -> moves
        liftIO $ writeIORef contextRef context{ sceneContextCameraMoves = moves' }
        return $ Just (createScene window worldRef contextRef)

-- Rotate the camera by dragging the mouse.
manipulate window worldRef contextRef _ (EventDrag dx dy) = do
    context <- liftIO $ readIORef contextRef
    let drag = (\(x, y) -> (x + realToFrac dx, y + realToFrac dy)) $ fromMaybe (0, 0) (sceneContextDrag context)
    liftIO $ writeIORef contextRef context{ sceneContextDrag = Just drag }
    return $ Just (createScene window worldRef contextRef)

-- Shot randomly colored fire balls with the mouse right button.
manipulate window worldRef contextRef size (EventMouseButton b bs _) = do
    world <- liftIO $ readIORef worldRef
    context <- liftIO $ readIORef contextRef
    let camera = fromJust (lookup (sceneContextCameraName context) (worldCameras world))
    newFireBalls <- if b == GLFW.MouseButton'2 && bs == GLFW.MouseButtonState'Pressed
        then do
            let (width, height) = size
                projectionMat = perspective (cameraFov camera) (width / height) (cameraNear camera) (cameraFar camera)
                cameraMat = lookAt'
                    (cameraPosition camera)
                    (cameraPosition camera + getSight camera)
                    (getUp camera)
                cursor = sceneContextCursorPosition context
                (V4 x y z _) = toWorld (width, height) (cameraNear camera, cameraNear camera) cursor projectionMat cameraMat
                direction = normalize (V3 x y z - cameraPosition camera)
            color <- liftIO $ runRandomIO $ V3
                <$> getRandomR (0, 1)
                <*> getRandomR (0, 1)
                <*> getRandomR (0, 1)
            return [FireBall (cameraPosition camera) direction color 0]
        else return []
    -- liftIO $ writeIORef worldRef world{ worldFireBalls = newFireBalls ++ worldFireBalls world }
    liftIO $ writeIORef contextRef context
    return $ Just (createScene window worldRef contextRef)

-- Store the cursor location.
manipulate window worldRef contextRef _ (EventCursorPos x y) = do
    context <- liftIO $ readIORef contextRef
    liftIO $ writeIORef contextRef context{ sceneContextCursorPosition = (realToFrac x, realToFrac y) }
    return $ Just (createScene window worldRef contextRef)

-- Catch everything else.
manipulate window worldRef contextRef _ _ =
    return $ Just (createScene window worldRef contextRef)

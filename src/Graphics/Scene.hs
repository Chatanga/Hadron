{-# language RankNTypes #-}
{-# language OverloadedStrings #-}
{-# language BlockArguments #-}

module Graphics.Scene
    ( Scene(..)
    , SceneName(..)
    , SceneContext(..)
    , createScene
    , createSceneContext
    ) where

import Control.Monad.State
import Control.Monad.Exception
import Control.Exception (evaluate)
import Data.Fixed
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import System.Log.Logger

import Common.Debug
import Common.Random
import Graphics.CubeRoom
import Graphics.Geometry
import Graphics.Incal
import Graphics.Polygonisation
import Graphics.RayMarching
import Graphics.Shaders
import Graphics.Texture
import Graphics.View
import Graphics.World

------------------------------------------------------------------------------------------------------------------------

data SceneName = Incal | Polygonisation | CubeRoom | RayMarching

data SceneContext m os = SceneContext
    { sceneContextCameraName :: !String
    , sceneContextCameraMoves :: !(Map.Map Move Double)
    , sceneContextCursorPosition :: !(Float, Float)
    , sceneContextDrag :: !(Maybe (Float, Float))
    , sceneContextRenderContext :: !(RenderContext m os)
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
    { sceneDisplay :: !(MonadIO m => ((Int, Int), (Int, Int)) -> ContextT GLFW.Handle os m ())
    , sceneAnimate :: !(MonadIO m => (Float, Float) -> Double -> ContextT GLFW.Handle os m (Scene m os))
    , sceneManipulate :: !(MonadIO m => (Float, Float) -> Event -> ContextT GLFW.Handle os m (Maybe (Scene m os))) -- ^ nothing => exit
    }

createScene :: (MonadIO m, MonadAsyncException m) => Window os f Depth -> IORef (World os) -> IORef (SceneContext m os) -> IORef Gui -> Scene m os
createScene window worldRef contextRef guiRef = Scene
    (display window worldRef contextRef guiRef)
    (animate window worldRef contextRef guiRef)
    (manipulate window worldRef contextRef guiRef)

createSceneContext :: (MonadIO m, MonadAsyncException m) => Window os RGBAFloat Depth -> SceneName -> String -> ContextT GLFW.Handle os m (SceneContext m os)
createSceneContext window sceneName cameraName = do
    renderer <- case sceneName of
        Incal -> createIncalRenderer window
        Polygonisation -> createPolygonisationRenderer window
        CubeRoom -> createCubeRoomRenderer window
        RayMarching -> createRayMarchingRenderer window
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
    -> IORef Gui
    -> ((Int, Int), (Int, Int))
    -> ContextT GLFW.Handle os m ()
display window worldRef contextRef guiRef bounds = do
    world <- liftIO $ readIORef worldRef
    context <- liftIO $ readIORef contextRef
    gui <- liftIO $ readIORef guiRef

    let camera = fromJust (lookup (sceneContextCameraName context) (worldCameras world))
        cameras = map snd (worldCameras world)
        sun = worldSun world
        lights = map (\(FireBall p _ c _) -> PointLight p c) (worldFireBalls world)
        buffers = worldBuffers world
        normalBuffers = worldNormalBuffers world
        renderContext = sceneContextRenderContext context
        cursorPosition = sceneContextCursorPosition context

    newRenderContext <- renderContextRenderAction renderContext
        renderContext
        bounds
        camera
        cameras
        sun
        lights
        buffers
        normalBuffers
        gui
        cursorPosition

    liftIO $ writeIORef contextRef (context{ sceneContextRenderContext = newRenderContext })

------------------------------------------------------------------------------------------------------------------------

animate :: (MonadIO m, MonadAsyncException m)
    => Window os f Depth
    -> IORef (World os)
    -> IORef (SceneContext m os)
    -> IORef Gui
    -> (Float, Float)
    -> Double
    -> ContextT GLFW.Handle os m (Scene m os)
animate window worldRef contextRef guiRef (_, height) timeDelta = do
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
                position = foldl' applyMove (cameraPosition camera) keyMoves
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

        -- https://ro-che.info/articles/2015-05-28-force-list
        forceElements :: [a] -> ()
        forceElements = foldr seq ()

    liftIO $ do
        evaluate (forceElements cameras)
        writeIORef worldRef world{ worldCameras = cameras}
        writeIORef contextRef context{ sceneContextDrag = Nothing }
    return $ createScene window worldRef contextRef guiRef

------------------------------------------------------------------------------------------------------------------------

manipulate :: (MonadIO m, MonadAsyncException m)
    => Window os f Depth
    -> IORef (World os)
    -> IORef (SceneContext m os)
    -> IORef Gui
    -> (Float, Float)
    -> Event
    -> ContextT GLFW.Handle os m (Maybe (Scene m os))

-- Handle keyboard events.
manipulate window worldRef contextRef guiRef _ (EventKey k _ ks _)

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
        return $ Just (createScene window worldRef contextRef guiRef)

    -- Save intermediate buffer rendering into a file.
    | k == GLFW.Key'O && ks == GLFW.KeyState'Pressed = do
        context <- liftIO $ readIORef contextRef
        let Just (_, frameBufferGroup) = renderContextFrameBufferGroup (sceneContextRenderContext context)
            tex = frameBufferGroupPositionTex frameBufferGroup
            V2 w h = head (texture2DSizes tex)
        saveTexture (w, h) tex "position.png"
        liftIO $ infoM "Hadron" "Color buffer saved"
        return $ Just (createScene window worldRef contextRef guiRef)

    -- Next camera.
    | k == GLFW.Key'Tab && ks == GLFW.KeyState'Pressed = do
        world <- liftIO $ readIORef worldRef
        context <- liftIO $ readIORef contextRef
        let cameraName = sceneContextCameraName context
            nextCameraName = do
                let names = map fst (worldCameras world)
                i <- elemIndex cameraName names
                return $ cycle names !! (i + 1)
        case nextCameraName of
            Just name -> liftIO $ writeIORef contextRef context{ sceneContextCameraName = name }
            Nothing -> return ()
        return $ Just (createScene window worldRef contextRef guiRef)

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
        return $ Just (createScene window worldRef contextRef guiRef)

-- Rotate the camera by dragging the mouse.
manipulate window worldRef contextRef guiRef _ (EventDrag dx dy) = do
    context <- liftIO $ readIORef contextRef
    let drag = (\(x, y) -> (x + realToFrac dx, y + realToFrac dy)) $ fromMaybe (0, 0) (sceneContextDrag context)
    liftIO $ writeIORef contextRef context{ sceneContextDrag = Just drag }
    return $ Just (createScene window worldRef contextRef guiRef)

-- Shot randomly colored fire balls with the mouse right button.
manipulate window worldRef contextRef guiRef size (EventMouseButton b bs _) | b == GLFW.MouseButton'2 = do
    world <- liftIO $ readIORef worldRef
    context <- liftIO $ readIORef contextRef
    let camera = fromJust (lookup (sceneContextCameraName context) (worldCameras world))

    newFireBalls <- if bs == GLFW.MouseButtonState'Pressed
        then do
            let (w, h) = size
                cursor = sceneContextCursorPosition context
                (_, direction) = calculatePickingRay (round w, round h) camera cursor
            color <- liftIO $ runRandomIO $ V3
                <$> getRandomR (0, 1)
                <*> getRandomR (0, 1)
                <*> getRandomR (0, 1)
            return [FireBall (cameraPosition camera) direction color 0]
        else return []

    -- liftIO $ writeIORef worldRef world{ worldFireBalls = newFireBalls ++ worldFireBalls world }
    return $ Just (createScene window worldRef contextRef guiRef)

-- Store the cursor location.
manipulate window worldRef contextRef guiRef _ (EventCursorPos x y) = do
    context <- liftIO $ readIORef contextRef
    liftIO $ writeIORef contextRef context{ sceneContextCursorPosition = (realToFrac x, realToFrac y) }
    return $ Just (createScene window worldRef contextRef guiRef)

-- Catch everything else.
manipulate window worldRef contextRef guiRef _ _ =
    return $ Just (createScene window worldRef contextRef guiRef)

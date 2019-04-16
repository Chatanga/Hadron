{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}

module Graphics.Scene
    ( Scene(..)
    , SceneContext(..)
    , createScene
    , createSceneContext
    ) where

import Control.Monad.State
import "lens" Control.Lens

import Data.Fixed
import Data.IORef
import Data.Maybe
import qualified Data.Set as Set

import Linear
import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
import System.Log.Logger

import Common.Random

import Graphics.Geometry
import Graphics.View
import Graphics.Shader
import Graphics.World
import Graphics.Texture

------------------------------------------------------------------------------------------------------------------------

data SceneContext os = SceneContext
    { contextCameraName :: String
    , contextCameraMoves :: !(Set.Set Move)
    , contextCursorPosition :: (Float, Float)
    , contextDrag :: Maybe (Float, Float)
    }

data Move
    = GoUp
    | GoDown
    | GoLeft
    | GoRight
    | GoForth
    | GoBack
    deriving (Eq, Ord, Show)

data Scene os = Scene
    { sceneDisplay :: ((Int, Int), (Int, Int)) -> ContextT GLFW.Handle os IO ()
    , sceneAnimate :: (Float, Float) -> Double -> ContextT GLFW.Handle os IO (Scene os)
    , sceneManipulate :: (Float, Float) -> Event -> ContextT GLFW.Handle os IO (Maybe (Scene os)) -- ^ nothing => exit
    }

createScene :: Window os RGBAFloat Depth -> IORef (World os) -> IORef (SceneContext os) -> Scene os
createScene window worldRef contextRef = Scene
    (display window worldRef contextRef)
    (animate window worldRef contextRef)
    (manipulate window worldRef contextRef)

createSceneContext :: Window os RGBAFloat Depth -> ContextT GLFW.Handle os IO (SceneContext os)
createSceneContext _ = return $ SceneContext
    "first-camera"
    Set.empty
    (0, 0)
    Nothing

------------------------------------------------------------------------------------------------------------------------

display :: Window os RGBAFloat Depth
    -> IORef (World os)
    -> IORef (SceneContext os)
    -> ((Int, Int), (Int, Int))
    -> ContextT GLFW.Handle os IO ()
display window worldRef contextRef size = do
    world <- liftIO $ readIORef worldRef

    fireBallObjects <- concat <$> mapM toRenderables (worldFireBalls world)

    let lights = map (\(FireBall p _ c _) -> PointLight p c) (worldFireBalls world)
        renderables = worldRenderables world

    displayScene window world contextRef size lights (fireBallObjects ++ renderables)

displayScene :: Window os RGBAFloat Depth
    -> (World os)
    -> IORef (SceneContext os)
    -> ((Int, Int), (Int, Int))
    -> [PointLight]
    -> [ViewPort -> Stage -> Render os ()]
    -> ContextT GLFW.Handle os IO ()
displayScene _ world contextRef bounds lights renderables = do
    context <- liftIO $ readIORef contextRef
    shadowMat <- renderShadow world context renderables
    renderDirectly world context bounds shadowMat lights renderables

renderShadow :: (World os)
    -> SceneContext os
    -> [ViewPort -> Stage -> Render os ()]
    -> ContextT GLFW.Handle os IO (M44 Float)
renderShadow world context renderables = do
    let
        (_, shadowTexSize) = worldDepthTex world
        camera = fromJust (lookup (contextCameraName context) (worldCameras world))
        sun = worldSun world
        r = far / 50
        projectionMat = ortho (-r) r (-r) r (-r) r
        localPosition = V3 0 0 0
        cameraMat = lookAt'
            localPosition
            (directionLightDirection sun)
            (getUp camera)
        biasMat = V4
            (V4 0.5 0.0 0.0 0.5)
            (V4 0.0 0.5 0.0 0.5)
            (V4 0.0 0.0 0.5 0.5)
            (V4 0.0 0.0 0.0 1.0)
        -- shadowMat = biasMat !*! projectionMat !*! cameraMat
        shadowMat = projectionMat !*! cameraMat

    writeBuffer (worldShaderConfigUniformBuffer world) 0 [ShaderConfig
        (cameraPosition camera)
        projectionMat
        cameraMat
        identity -- Transformation
        1 -- ShadowUsed
        shadowMat
        (Fog (V4 0.5 0.5 0.5 1) 10 100 0.2)
        (worldSun world)
        0 -- TimePassed
        ]

    writeBuffer (worldPointLightUniformBuffer world) 0 []

    let viewPort = ViewPort (V2 0 0) shadowTexSize
    mapM_ (\r -> render (r viewPort ShadowMappingStage)) renderables

    return shadowMat

renderDirectly :: (World os)
    -> SceneContext os
    -> ((Int, Int), (Int, Int))
    -> M44 Float
    -> [PointLight]
    -> [ViewPort -> Stage -> Render os ()]
    -> ContextT GLFW.Handle os IO ()
renderDirectly world context bounds shadowMat lights renderables = do
    let
        ((x, y), (w, h)) = bounds
        camera = fromJust (lookup (contextCameraName context) (worldCameras world))
        -- FOV (y direction, in radians), Aspect ratio, Near plane, Far plane
        projectionMat = perspective (cameraFov camera) (fromIntegral w / fromIntegral h) near far
        -- Eye, Center, Up
        cameraMat = lookAt
            (cameraPosition camera)
            (cameraPosition camera + getSight camera)
            (getUp camera)

    writeBuffer (worldShaderConfigUniformBuffer world) 0 [ShaderConfig
        (cameraPosition camera)
        projectionMat
        cameraMat
        identity -- Transformation
        0 -- ShadowUsed
        shadowMat
        (Fog (V4 0.5 0.5 0.5 1) 10 100 0.2)
        (worldSun world)
        0 -- TimePassed
        ]

    writeBuffer (worldPointLightUniformBuffer world) 0 lights

    let viewPort = ViewPort (V2 x y) (V2 w h)
    -- See https://github.com/tobbebex/GPipe-Core/issues/50
    -- render $ mapM_ (\r -> r viewPort) renderables
    mapM_ (\r -> render (r viewPort DirectShadingStage)) renderables

-- Copy of lookAt from linear with normalize replaced with signorm (faster? or
-- without the epsilon constraint which is not fulfilled in shaders?).
lookAt' :: Floating a => V3 a -> V3 a -> V3 a -> V4 (V4 a)
lookAt' eye center up =
    V4 (V4 (xa^._x)  (xa^._y)  (xa^._z)  xd)
        (V4 (ya^._x)  (ya^._y)  (ya^._z)  yd)
        (V4 (-za^._x) (-za^._y) (-za^._z) zd)
        (V4 0         0         0          1)
    where
        za = signorm $ center - eye
        xa = signorm $ cross za up
        ya = cross xa za
        xd = -dot xa eye
        yd = -dot ya eye
        zd = dot za eye

------------------------------------------------------------------------------------------------------------------------

animate :: Window os RGBAFloat Depth
    -> IORef (World os)
    -> IORef (SceneContext os)
    -> (Float, Float)
    -> Double
    -> ContextT GLFW.Handle os IO (Scene os)
animate window worldRef contextRef (_, height) timeDelta = do
    world <- liftIO $ readIORef worldRef
    context <- liftIO $ readIORef contextRef

    -- Move a camera by applying any registered move for the given time delta.
    let moveCamera camera = camera
                { cameraAltitude = alt'
                , cameraAzimuth = az'
                , cameraPosition = position }
            where
                moves = contextCameraMoves context
                keyMoves =
                    [ (GoUp, getUp camera)
                    , (GoDown, - (getUp camera))
                    , (GoLeft, getLeft camera)
                    , (GoRight, - (getLeft camera))
                    , (GoForth, getSight camera)
                    , (GoBack, - (getSight camera))
                    ]
                applyMove p (m, dp) = if Set.member m moves
                    then p + dp * realToFrac timeDelta * 10
                    else p
                position = foldl applyMove (cameraPosition camera) keyMoves
                (alt, az) = (cameraAltitude camera, cameraAzimuth camera)
                (alt', az') = case contextDrag context of
                    Nothing -> (alt, az)
                    Just (dx, dy) -> (alt', az') where
                        ratio = cameraFov camera / realToFrac height
                        (dAz, dAlt) = (dx * ratio, dy * ratio)
                        alt' = alt + clamp (-pi/2.01-alt) (pi/2.01-alt) dAlt
                        az' = Data.Fixed.mod' (az - dAz) (2 * pi)

    let for = flip map
        cameras = for (worldCameras world) $ \(n, c) ->
            if n == contextCameraName context then (n, moveCamera c) else (n, c)

    liftIO $ writeIORef worldRef world{ worldCameras = cameras }
    liftIO $ writeIORef contextRef context{ contextDrag = Nothing }
    return $ createScene window worldRef contextRef

------------------------------------------------------------------------------------------------------------------------

manipulate :: Window os RGBAFloat Depth
    -> IORef (World os)
    -> IORef (SceneContext os)
    -> (Float, Float)
    -> Event
    -> ContextT GLFW.Handle os IO (Maybe (Scene os))

-- Handle keyboard events.
manipulate window worldRef contextRef _ (EventKey k _ ks _)

    -- Exit application on Escape.
    | k == GLFW.Key'Escape = return Nothing

    -- Exit application on Escape.
    | k == GLFW.Key'P && ks == GLFW.KeyState'Pressed = do
        world <- liftIO $ readIORef worldRef
        let (tex, (V2 w h)) = worldDepthTex world
        saveDepthTexture (w, h) tex "shadowmap.png"
        liftIO $ infoM "Hadron" "Shadow map saved"
        return $ Just (createScene window worldRef contextRef)

    -- Move the camera using WASD keys (note that the keyboard layout is not taken into account).
    | otherwise = do
        context <- liftIO $ readIORef contextRef
        let
            moves = contextCameraMoves context
            handleKey = if ks == GLFW.KeyState'Released then Set.delete else Set.insert
            moves' = case k of
                GLFW.Key'Space -> handleKey GoUp moves
                GLFW.Key'LeftControl -> handleKey GoDown moves
                GLFW.Key'W -> handleKey GoForth moves
                GLFW.Key'S -> handleKey GoBack moves
                GLFW.Key'A -> handleKey GoLeft moves
                GLFW.Key'D -> handleKey GoRight moves
                _ -> moves
        liftIO $ writeIORef contextRef context{ contextCameraMoves = moves' }
        return $ Just (createScene window worldRef contextRef)

-- Rotate the camera by dragging the mouse.
manipulate window worldRef contextRef _ (EventDrag dx dy) = do
    context <- liftIO $ readIORef contextRef
    let drag = (\(x, y) -> (x + realToFrac dx, y + realToFrac dy)) $ fromMaybe (0, 0) (contextDrag context)
    liftIO $ writeIORef contextRef context{ contextDrag = Just drag }
    return $ Just (createScene window worldRef contextRef)

-- Shot randomly colored fire balls with the mouse right button.
manipulate window worldRef contextRef size (EventMouseButton b bs _) = do
    world <- liftIO $ readIORef worldRef
    context <- liftIO $ readIORef contextRef
    let camera = fromJust (lookup (contextCameraName context) (worldCameras world))
    newFireBalls <- if b == GLFW.MouseButton'2 && bs == GLFW.MouseButtonState'Pressed
        then do
            let (width, height) = size
                projectionMat = perspective (cameraFov camera) (width / height) near far
                cameraMat = lookAt'
                    (cameraPosition camera)
                    (cameraPosition camera + getSight camera)
                    (getUp camera)
                cursor = contextCursorPosition context
                (V4 x y z _) = toWorld (width, height) cursor projectionMat cameraMat
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
    liftIO $ writeIORef contextRef context{ contextCursorPosition = (realToFrac x, realToFrac y) }
    return $ Just (createScene window worldRef contextRef)

-- Catch everything else.
manipulate window worldRef contextRef _ _ =
    return $ Just (createScene window worldRef contextRef)

{-# language FlexibleContexts  #-}
{-# language RankNTypes #-}
{-# language BlockArguments #-}
{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module Graphics.Application
    ( runApplication
    ) where

import Control.Lens ((&), (<&>), (^.), index)
import Control.Monad
import Control.Monad.State
import Data.IORef
import Data.Maybe
import Data.StateVar
import Data.Tree
import Data.Tree.Zipper
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import Numeric
import System.Log.Logger

import Control.Monad.IO.Class
import Control.Monad.Exception

import Control.Monad.Managed
import Data.Bits ((.|.))
import Data.List (sortBy)
import Data.Foldable (traverse_)
import Data.Text (Text, pack)

import Common.Debug
import Graphics.Geometry
import Graphics.Scene
import Graphics.Shaders
import Graphics.View
import Graphics.World

import DearImGui
import DearImGui.OpenGL3
import DearImGui.GLFW
import DearImGui.GLFW.OpenGL
import Graphics.GL
import qualified Graphics.UI.GLFW

------------------------------------------------------------------------------------------------------------------------

createViewWithParams :: (MonadIO m, MonadAsyncException m)
    => Bool
    -> Layout (ContextT GLFW.Handle os m) (Maybe (Scene m os))
    -> ViewHandleEvent (ContextT GLFW.Handle os m) (Maybe (Scene m os))
    -> Maybe (Scene m os)
    -> View (ContextT GLFW.Handle os m) (Maybe (Scene m os))
createViewWithParams hasFocus layout handleEvent content = (createView' content)
    { viewHandleEvent = handleEvent
    , viewLayout = layout
    , viewHasFocus = hasFocus
    }
    where
        createView' :: (MonadIO m, MonadAsyncException m) => Maybe (Scene m os) -> View (ContextT GLFW.Handle os m) (Maybe (Scene m os))
        createView' = createView

{- Handle an event through simple delegation to its inner content scene, if any.
A scene being not aware of the view holding it, any change will stay local
(excepted for a Nothing interpreted as a closure).
-}
sceneHandleEvent :: (MonadIO m, MonadAsyncException m) => ViewHandleEvent (ContextT GLFW.Handle os m) (Maybe (Scene m os))
sceneHandleEvent event treeLoc =
    let view = getLabel treeLoc
        (_, (w, h)) = viewLocalBounds view
    in case viewContent view of
        Nothing -> return BubbleUp
        Just scene -> do
            content' <- sceneManipulate scene (fromIntegral w, fromIntegral h) event
            case content' of
                Nothing -> return Terminate
                Just scene' ->
                    let view' = view{ viewContent = Just scene' }
                    in  return (Consume (modifyLabel (const view') treeLoc))

------------------------------------------------------------------------------------------------------------------------

type ScenicUI m os = UI (ContextT GLFW.Handle os m) (Maybe (Scene m os))

createScenicUI :: (MonadIO m, MonadAsyncException m)
    => Graphics.GPipe.Window os f Depth
    -> IORef (World os)
    -> IORef (SceneContext m os)
    -> IORef Gui
    -> ScenicUI m os
createScenicUI window currentWorld currentContext currentGui = createUI ui where
    masterScene = createScene window currentWorld currentContext currentGui
    handleEvent = sceneHandleEvent
    ui = Node (createViewWithParams True defaultLayout handleEvent (Just masterScene)) []

{- Animate all scenes into the UI. Note that animating a scene is not the same
thing as animating the world displayed by the scene. Worlds are animated
separately.
-}
animateUI :: (MonadIO m, MonadAsyncException m)
    => Double
    -> ScenicUI m os -> ContextT GLFW.Handle os m (ScenicUI m os)
animateUI frameDuration ui = do
    root' <- forM (uiRoot ui) $ \view -> do
        let (_, (w, h)) = viewLocalBounds view
            size = (fromIntegral w, fromIntegral h)
        c <- case viewContent view of
            Just scene -> Just <$!> sceneAnimate scene size frameDuration
            Nothing -> return Nothing
        return view{ viewContent = c }
    return $ ui{ uiRoot = root' }

{- Recursively display the tree of scenes.
-}
renderViewTree :: (MonadIO m, MonadAsyncException m)
    => (Int, Int)
    -> Tree (View (ContextT GLFW.Handle os m) (Maybe (Scene m os)))
    -> ContextT GLFW.Handle os m ()
renderViewTree screenSize viewTree = do
    let (_, screenHeigh) = screenSize
        view = rootLabel viewTree
    case viewContent view of
        Just scene -> do
            let ((x, y), (w, h)) = viewLocalBounds view
            sceneDisplay scene ((x, screenHeigh - h - y), (w, h))
        _ -> return ()
    mapM_ (renderViewTree screenSize) (subForest viewTree)

{-
instance MonadException Managed where
    throw e = error (show e)
    catch x _ = x

instance MonadAsyncException Managed where
    mask x = x id

runApplication :: String -> IO ()
runApplication name = runManaged $ do
    runApplication' name

runApplication' :: String -> Managed ()
runApplication' name = runContextT (GLFW.defaultHandleConfig{ GLFW.configOpenGlVersion = GLFW.OpenGlVersion 4 5 } ) $ do
-}

data GuiContent = GuiContent
  { guiContentContext :: Maybe Context
  , guiContentBuilder :: IO ()
  }

activateDearImGui :: IORef GuiContent -> IO ()
activateDearImGui guiContentRef = do
    Just win <- Graphics.UI.GLFW.getCurrentContext
    guiContent <- readIORef guiContentRef

    context <- case guiContentContext guiContent of
        Nothing -> do
            -- Create an ImGui context
            context <- DearImGui.createContext -- / destroyContext
            -- Initialize ImGui's GLFW backend
            {-
            TODO Rerouting the event won’t be easy since DearImGui callback are low level.

            glfwCursorPosCallback
            glfwMouseButtonCallback
            glfwKeyCallback

            glfwWindowFocusCallback
            glfwCursorEnterCallback
            glfwCharCallback
            glfwScrollCallback
            glfwMonitorCallback
            -}
            glfwInitForOpenGL win True -- / glfwShutdown
            -- Initialize ImGui's OpenGL backend
            openGL3Init -- / openGL3Shutdown

            writeIORef guiContentRef (guiContent { guiContentContext = Just context })

            return context

        Just context -> return context

    -- Tell ImGui we're starting a new frame
    openGL3NewFrame
    glfwNewFrame
    newFrame

    -- Build the GUI
    guiContentBuilder guiContent

    -- Render
    DearImGui.render
    openGL3RenderDrawData =<< getDrawData

data DataPath c a = DataPath {
    ref :: IORef c,
    getIt :: c -> a,
    setIt :: c -> a -> c
}

instance HasGetter (DataPath c a) a where
    get p = liftIO $ getIt p <$> readIORef (ref p)

instance HasSetter (DataPath c a) a where
    p $= x = liftIO $ modifyIORef (ref p) (\c -> setIt p c x)

buildDearImGui :: IORef (World os) -> IORef (SceneContext IO os) -> IORef Gui -> IO ()
buildDearImGui worldRef contextRef guiRef = do
    let readOnly x _ = x
    let fpsPath = DataPath guiRef (pack . (\n -> showGFloat (Just 2) n "") . guiFps) readOnly
        debugPath = DataPath guiRef guiDebug (\g x -> g{ guiDebug = x })
        fogDensity = DataPath guiRef guiFogDensity (\g x -> g{ guiFogDensity = x })
        [xPath, yPath, zPath] = map
            (\c -> DataPath worldRef (pack . show . c . cameraPosition . snd . head . worldCameras) readOnly)
            [(^._x), (^._y) ,(^._z)]
    bracket_ (begin "Hadron - Settings") end do
        inputText "FPS" fpsPath 0
        text "First camera position"
        inputText "x" xPath 0
        inputText "y" yPath 0
        inputText "z" yPath 0
        text "Fog"
        sliderFloat "density" fogDensity 0 0.005
        text "Misc"
        checkbox "Debug" debugPath
        {-
        clicking <- button "Close"
        when clicking $ Data.StateVar.get debugPath >>= print
        -}
        return ()

runApplication :: String -> IO ()
runApplication name = do
    guiContentRef <- newIORef (GuiContent Nothing (return ()))
    let config = GLFW.defaultHandleConfig
            { GLFW.configOpenGlVersion = GLFW.OpenGlVersion 4 5
            , GLFW.configWindowHook = Just (activateDearImGui guiContentRef)
            }
    runContextT config $ do
        let (w , h) = (800, 600)

        window <- newWindow
            (WindowFormatColorDepth SRGB8A8 Depth16)
            (GLFW.defaultWindowConfig name){ GLFW.configWidth = w, GLFW.configHeight = h }

        currentWorld <- liftIO . newIORef =<< createWorld window
        currentContext <- liftIO . newIORef =<< createSceneContext window Polygonisation "primary-camera"
        currentGui <- liftIO $ newIORef (Gui 0 0.002 false)
        uiRef <- liftIO . newIORef $ createScenicUI window currentWorld currentContext currentGui
        quitRef <- liftIO $ newIORef False

        eventQueueRef <- liftIO $ newIORef []
        let
            -- Queue an event for later processing in the proper context.
            pushEvent event = modifyIORef' eventQueueRef (event :)
            -- Process an event in the proper context.
            doProcessEvent event = do
                ui <- liftIO $ readIORef uiRef
                ui' <- processEvent ui event
                liftIO $ writeIORef uiRef ui'
                liftIO $ writeIORef quitRef (uiTerminated ui')

        -- Collect and queue incoming events.
        GLFW.setKeyCallback window $ Just $ \k n ks mk -> do
            pushEvent (EventKey k n ks mk)
        GLFW.setMouseButtonCallback window $ Just $ \b bs mk ->
            pushEvent (EventMouseButton b bs mk)
        void $ GLFW.setCursorPosCallback window $ Just $ \x y ->
            pushEvent (EventCursorPos x y)

        liftIO $ do
            guiContent <- readIORef guiContentRef
            writeIORef guiContentRef (guiContent { guiContentBuilder = buildDearImGui currentWorld currentContext currentGui })

        -- Run the main loop.
        mainLoop window 0 (0, Nothing, Nothing) (V2 0 0) currentWorld currentContext currentGui uiRef quitRef eventQueueRef doProcessEvent

mainLoop :: Window os f Depth
    -> Int
    -> (Int, Maybe Double, Maybe Double)
    -> V2 Int
    -> IORef (World os)
    -> IORef (SceneContext m os)
    -> IORef Gui
    -> IORef (ScenicUI IO os)
    -> IORef Bool
    -> IORef [Event]
    -> (Event -> ContextT GLFW.Handle os IO ())
    -> ContextT GLFW.Handle os IO ()
mainLoop window counter (frameCount, mt0, mt1) bfSize worldRef contextRef guiRef uiRef quitRef eventQueueRef doProcessEvent = do

    -- Calculate the FPS.
    mt2 <- liftIO GLFW.getTime
    let elapsedSeconds = fromMaybe 1 ((-) <$> mt2 <*> mt0)
    timing <- if elapsedSeconds > 0.25
        then do
            let fps = fromIntegral frameCount / elapsedSeconds
            -- liftIO $ debugM "Hadron" ("FPS: " ++ showGFloat (Just 2) fps "")
            liftIO $ modifyIORef guiRef (\gui -> gui{ guiFps = fps })
            return (1, mt2, mt2)
        else
            return (frameCount + 1, mt0, mt2)

    bfSize'@(V2 w h) <- getFrameBufferSize window
    when (bfSize /= bfSize') $ liftIO $ modifyIORef' uiRef (layout (w, h))

    -- Process the event queue.
    eventQueue <- liftIO $ readIORef eventQueueRef
    liftIO $ writeIORef eventQueueRef []
    mapM_ doProcessEvent (reverse eventQueue)

    -- Animate both the world and the scenes (in the UI).
    let frameDuration = fromMaybe 0 ((-) <$> mt2 <*> mt1)
    liftIO (readIORef worldRef) >>= animateWorld frameDuration >>= (liftIO . writeIORef worldRef)
    liftIO (readIORef uiRef) >>= animateUI frameDuration >>= (liftIO . writeIORef uiRef)

    -- Render the frame.
    -- render $ clearWindowColor window 0.5
    ui <- liftIO $ readIORef uiRef
    when (w > 0) $ do -- avoid an upcoming divide by zero
        renderViewTree (w, h) (uiRoot ui)

    swapWindowBuffers window

    -- Do the whole thing again?
    shouldClose <- fromMaybe False <$> GLFW.windowShouldClose window
    shouldQuit <- (||) <$> liftIO (readIORef quitRef) <*> pure shouldClose
    if shouldQuit
        then liftIO $ infoM "Hadron" "Exiting"
        else mainLoop window (counter+1) timing bfSize' worldRef contextRef guiRef uiRef quitRef eventQueueRef doProcessEvent

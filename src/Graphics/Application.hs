{-# LANGUAGE FlexibleContexts  #-}
{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language InstanceSigs #-}

module Graphics.Application
    ( runApplication
    ) where

import Control.Monad
import Control.Monad.State
import Data.IORef
import Data.Maybe
import Data.Tree
import Data.Tree.Zipper
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import Numeric
import System.Log.Logger

import Control.Monad.IO.Class
import Control.Monad.Exception

{-
import Control.Monad.Managed
import qualified DearImGui as ImGui
import qualified DearImGui.OpenGL3 as ImGui
-}

import Common.Debug
import Graphics.Scene
import Graphics.View
import Graphics.World

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
    => Window os f Depth
    -> IORef (World os)
    -> IORef (SceneContext m os)
    -> ScenicUI m os
createScenicUI window currentWorld currentContext = createUI ui where
    masterScene = createScene window currentWorld currentContext
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

------------------------------------------------------------------------------------------------------------------------

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

runApplication :: String -> IO ()
runApplication name = runContextT (GLFW.defaultHandleConfig{ GLFW.configOpenGlVersion = GLFW.OpenGlVersion 4 5 } ) $ do
    let (w , h) = (800, 600)

    window <- newWindow
        (WindowFormatColorDepth SRGB8A8 Depth16)
        (GLFW.defaultWindowConfig name){ GLFW.configWidth = w, GLFW.configHeight = h }

    currentWorld <- liftIO . newIORef =<< createWorld window
    currentContext <- liftIO . newIORef =<< createSceneContext window Polygonisation "primary-camera"
    uiRef <- liftIO . newIORef $ createScenicUI window currentWorld currentContext
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
    GLFW.setKeyCallback window $ Just $ \k n ks mk ->
        pushEvent (EventKey k n ks mk)
    GLFW.setMouseButtonCallback window $ Just $ \b bs mk ->
        pushEvent (EventMouseButton b bs mk)
    void $ GLFW.setCursorPosCallback window $ Just $ \x y ->
        pushEvent (EventCursorPos x y)

    -- Run the main loop.
    mainLoop window 0 (0, Nothing, Nothing) (V2 0 0) currentWorld uiRef quitRef eventQueueRef doProcessEvent

mainLoop :: Window os f Depth
    -> Int
    -> (Int, Maybe Double, Maybe Double)
    -> V2 Int
    -> IORef (World os)
    -> IORef (ScenicUI IO os)
    -> IORef Bool
    -> IORef [Event]
    -> (Event -> ContextT GLFW.Handle os IO ())
    -> ContextT GLFW.Handle os IO ()
mainLoop window counter (frameCount, mt0, mt1) bfSize worldRef uiRef quitRef eventQueueRef doProcessEvent = do

    -- Calculate the FPS.
    mt2 <- liftIO GLFW.getTime
    let elapsedSeconds = fromMaybe 1 ((-) <$> mt2 <*> mt0)
    timing <- if elapsedSeconds > 0.25
        then do
            let fps = fromIntegral frameCount / elapsedSeconds
            liftIO $ debugM "Hadron" ("FPS: " ++ showGFloat (Just 2) fps "")
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
        else mainLoop window (counter+1) timing bfSize' worldRef uiRef quitRef eventQueueRef doProcessEvent

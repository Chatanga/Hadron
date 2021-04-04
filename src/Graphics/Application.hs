{-# LANGUAGE PackageImports, FlexibleContexts  #-}

module Graphics.Application
    ( runApplication
    ) where

import Control.Monad
import Control.Monad.State

import Data.IORef
import Data.Maybe
import Data.Tree
import Data.Tree.Zipper

import Numeric

import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
import System.Log.Logger

import Graphics.Layouts
import Graphics.Scene
import Graphics.View
import Graphics.World

------------------------------------------------------------------------------------------------------------------------

createViewWithParams :: Bool
    -> Layout (ContextT GLFW.Handle os IO) (Maybe (Scene os))
    -> ViewHandleEvent (ContextT GLFW.Handle os IO) (Maybe (Scene os))
    -> Maybe (Scene os)
    -> View (ContextT GLFW.Handle os IO) (Maybe (Scene os))
createViewWithParams hasFocus layout handleEvent content = (createView' content)
    { viewHandleEvent = handleEvent
    , viewLayout = layout
    , viewHasFocus = hasFocus
    }
    where
        createView' :: Maybe (Scene os) -> View (ContextT GLFW.Handle os IO) (Maybe (Scene os))
        createView' = createView

{- Handle an event through simple delegation to its inner content scene, if any.
A scene being not aware of the view holding it, any change will stay local
(excepted for a Nothing interpreted as a closure).
-}
sceneHandleEvent :: ViewHandleEvent (ContextT GLFW.Handle os IO) (Maybe (Scene os))
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

type ScenicUI os = UI (ContextT GLFW.Handle os IO) (Maybe (Scene os))

createScenicUI :: Window os f Depth
    -> IORef (World os)
    -> IORef (SceneContext os)
    -> ScenicUI os
createScenicUI window currentWorld currentContext = createUI ui where
    masterScene = createScene window currentWorld currentContext
    radarScene = createScene window currentWorld currentContext

    handleEvent = sceneHandleEvent

    ui =
        Node (createViewWithParams False adaptativeLayout handleEvent Nothing)
        [   Node (createViewWithParams True (anchorLayout topRighCorner) handleEvent (Just masterScene))
            [   -- Node (createViewWithParams False (fixedLayout (250, 250)) handleEvent  (Just radarScene)) []
            ]
        ]
    topRighCorner = [AnchorConstraint (Just 50) (Just 50) Nothing Nothing]

{- Animate all scenes into the UI. Note that animating a scene is not the same
thing as animating the world displayed by the scene. Worlds are animated
separately.
-}
animateUI :: Double -> ScenicUI os -> ContextT GLFW.Handle os IO (ScenicUI os)
animateUI frameDuration ui = do
    root' <- forM (uiRoot ui) $ \view -> do
        let (_, (w, h)) = viewLocalBounds view
            size = (fromIntegral w, fromIntegral h)
        c <- case viewContent view of
            Just scene -> Just <$> sceneAnimate scene size frameDuration
            Nothing -> return Nothing
        return view{ viewContent = c }
    return $ ui{ uiRoot = root' }

{- Recursively display the tree of scenes.
-}
renderViewTree :: (Int, Int)
    -> Tree (View (ContextT GLFW.Handle os IO) (Maybe (Scene os)))
    -> ContextT GLFW.Handle os IO ()
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

runApplication :: String -> IO ()
runApplication name = runContextT GLFW.defaultHandleConfig $ do
    let (w , h) = (800, 600)

    window <- newWindow
        (WindowFormatColorDepth SRGB8A8 Depth16)
        (GLFW.defaultWindowConfig name){ GLFW.configWidth = w, GLFW.configHeight = h }

    currentWorld <- liftIO . newIORef =<< createWorld window
    currentContext <- liftIO . newIORef =<< createSceneContext window
    uiRef <- liftIO . newIORef $ createScenicUI window currentWorld currentContext
    quitRef <- liftIO $ newIORef False

    eventQueueRef <- liftIO $ newIORef []
    let
        -- Queue an event for later processing in the proper context.
        pushEvent event = modifyIORef eventQueueRef (event :)
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
    GLFW.setCursorPosCallback window $ Just $ \x y ->
        pushEvent (EventCursorPos x y)

    -- Run the main loop.
    mainLoop window (0, Nothing, Nothing) currentWorld uiRef quitRef eventQueueRef doProcessEvent

mainLoop :: Window os RGBAFloat Depth
    -> (Int, Maybe Double, Maybe Double)
    -> IORef (World os)
    -> IORef (ScenicUI os)
    -> IORef Bool
    -> IORef [Event]
    -> (Event -> ContextT GLFW.Handle os IO ())
    -> ContextT GLFW.Handle os IO ()
mainLoop window (frameCount, mt0, mt1) worldRef uiRef quitRef eventQueueRef doProcessEvent = do

    -- Calculate the FPS.
    mt2 <- liftIO GLFW.getTime
    let elapsedSeconds = fromMaybe 1 ((-) <$> mt2 <*> mt0)
    (_, timing) <- if elapsedSeconds > 0.25
        then do
            let fps = fromIntegral frameCount / elapsedSeconds
            liftIO $ debugM "Hadron" ("FPS: " ++ showGFloat (Just 2) fps "")
            return (Just fps, (0, mt2, mt2))
        else
            return (Nothing, (frameCount + 1, mt0, mt2))

    -- Update the UI to the screen size.
    (V2 w h) <- getFrameBufferSize window
    liftIO $ modifyIORef uiRef (layout (w, h))

    -- Process the event queue.
    eventQueue <- liftIO $ readIORef eventQueueRef
    liftIO $ writeIORef eventQueueRef []
    mapM_ doProcessEvent (reverse eventQueue)

    -- Animate both the world and the scenes (in the UI).
    let frameDuration = fromMaybe 0 ((-) <$> mt2 <*> mt1)
    liftIO (readIORef worldRef) >>= animateWorld frameDuration >>= (liftIO . writeIORef worldRef)
    liftIO (readIORef uiRef) >>= animateUI frameDuration >>= (liftIO . writeIORef uiRef)

    -- Render the frame.
    render $ clearWindowColor window 0.5
    ui <- liftIO $ readIORef uiRef
    when (w > 0) $ do -- avoid an upcoming divide by zero
        renderViewTree (w, h) (uiRoot ui)
    swapWindowBuffers window

    -- Do the whole thing again?
    shouldClose <- fromMaybe False <$> GLFW.windowShouldClose window
    shouldQuit <- (||) <$> liftIO (readIORef quitRef) <*> pure shouldClose
    if shouldQuit
        then liftIO $ infoM "Hadron" "Exiting"
        else mainLoop window timing worldRef uiRef quitRef eventQueueRef doProcessEvent

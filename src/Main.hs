module Main (
  main
) where

import Control.Concurrent
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Dependent.Sum
import Data.Foldable
import Data.Map
import Data.Maybe
import Data.IORef
import Data.Time.Clock
import Data.Traversable
import GHC.IORef
import Reflex hiding (mapMaybe)
import Reflex.Host.Class
import System.Exit
import System.Mem

import App
import App.Class
import App.UI
import App.Graphics as Graphics

name :: String
name = "Acts of Enclosure"

version :: String
version = "0.1.0"

data InputEvent =
         KeyEvent Key KeyState
       | MouseButtonEvent MouseButton MouseButtonState

main :: IO ()
main = do
  putStrLn   ""
  putStrLn $ name ++ " — version " ++ version ++ " ..."
  putStrLn   ""
  putStrLn   "Copyright Categorical Industries 2024, All Rights Reserved."
  putStrLn   ""

  putStrLn "Starting..."

  eventsChan <- newChan

  -- NB: The dollar keeps the type checker happy, otherwise the Rank-2 type
  -- parameter in runContextT causes a type error.
  runSpiderHost $ runContextT defaultHandleConfig $ do
    (window, windowSize, render, uiElemIdTexture) <-
      Graphics.initialise name

    -- Create post build event.
    (ePostBuild, postBuildTriggerRef) <- newEventWithTriggerRef

    -- Create a tick event so the host can trigger a tick after each
    -- render.
    (eTick, tickTrigger) <- runTriggerEventT newTriggerEvent eventsChan

    -- The time the last tick occurred.
    lastTickRef <- liftIO $ newIORef =<< getCurrentTime

    -- Set callbacks for our GLFW window and store the events collected in the
    -- last frame.

    -- Input event store, written to in reverse order, so remember to reverse
    -- when processing.
    inputEventsRef :: IORef [InputEvent] <- liftIO . newIORef $ []

    _ <- setKeyCallback window . Just $ \k _ ks _ -> do
           modifyIORef inputEventsRef (KeyEvent k ks :)

    _ <- setMouseButtonCallback window . Just $ \b s _ -> do
           modifyIORef inputEventsRef (MouseButtonEvent b s :)


    -- Last mouse position stored for UI click processing.
    mousePosRef :: IORef (Int, Int) <- liftIO . newIORef $ (0, 0)
    -- Mouse move events - coalesced into the latest move in the frame
    mouseMoveEventRef :: IORef (Maybe (Int, Int))
      <- liftIO . newIORef $ Nothing

    _ <- setCursorPosCallback window . Just $ \x y -> do
           -- Round off any subpixel sampling
           let x' = round x
               y' = round y
           -- Store the mouse position for other events
           liftIO . writeIORef mousePosRef $ (x', y')
           liftIO . writeIORef mouseMoveEventRef . Just $ (x', y')

    -- Unhandled UI events
    --
    -- Create triggers that we can fire when we process inputs
    (eCursorPos, cursorPosTrigger) <-
      runTriggerEventT newTriggerEvent eventsChan
    (eUnhandledKey, unhandledKeyTrigger) <-
      runTriggerEventT newTriggerEvent eventsChan
    (eUnhandledMouseButton, unhandledMouseButtonTrigger) <-
      runTriggerEventT newTriggerEvent eventsChan

    let uiEvents' = UIEvents eCursorPos eUnhandledKey eUnhandledMouseButton

    -- General environment variables
    let env = Env eTick windowSize

    -- Create the reflex FRP network.
    (((eQuit, eTickOut, scene), ui, uiTriggerMap), FireCommand fire) <-
        hostPerformEventT
        . flip runPostBuildT ePostBuild
        . flip runTriggerEventT eventsChan
        . flip runReaderT env
        . runUiBuilderT uiEvents'
        $ game

    -- Event handles - for reading current `Event` values, which may or may not
    -- have been produced since the last frame.
    hQuit <- subscribeEvent eQuit
    hTick <- subscribeEvent eTickOut

    let processInputEvents = do
          -- Dispatch inputs to the right UI element.
          --
          -- Clicks and hovers are dispatched by sampling the element ID
          -- texture at the mouse position. 0 is a reserved value in the
          -- the texture which means that there's no element there.
          --
          -- Key presses will be dispatched the UI element has the current
          -- focus.
          --
          -- Unhandled clicks and key presses will be dispatched to the
          -- UIBuilderT where they can be handled by the rest of the app.

          -- Sample the element ID texture once per frame since we're
          -- coalescing all the cursor move events into the final one.
          (curX, curY) <- liftIO . readIORef $ mousePosRef
          -- Find the hovered element if there is one, 0 in the texture
          -- means nothing to click on
          elemId <- readUiElemIdTexturePixel uiElemIdTexture curX curY

          mMouseMoveEvent <- liftIO . readIORef $ mouseMoveEventRef
          case mMouseMoveEvent of
            Just (x, y) -> liftIO $ do
              writeIORef mouseMoveEventRef Nothing
              -- TODO trigger elem hover event
              cursorPosTrigger . P . V2 x $ y
            Nothing -> return ()

          -- Events were collected in reverse.
          events <- liftIO . fmap reverse . atomicSwapIORef inputEventsRef $ []
          forM_ events $ \case
            KeyEvent k ks         -> liftIO . unhandledKeyTrigger $ (k, ks)
            MouseButtonEvent b bs -> liftIO $ do
              case elemId of
                0 -> unhandledMouseButtonTrigger (b, bs)
                _ -> mapM_ ($ ()) $ uiTriggerMap !? elemId

    let doTick = liftIO $ do
          now' <- getCurrentTime
          deltaT <- fmap (realToFrac . diffUTCTime now') . readIORef
            $ lastTickRef
          tickTrigger deltaT
          writeIORef lastTickRef now'

    -- Processes event outputs and finds out if we should shutdown.
    -- There will be a list of outputs, one for each event triggered in the
    -- last frame so we have to process them all accordingly.
    let handleOutput outs = do
          let tickDone = any (isJust . snd) outs
          -- Any quit event should cause the application to exit.
              shutdown = any (isJust . fst) outs
          return (shutdown, tickDone)

    -- Reads output event handles and sequences any actions by PerformEvent
    -- methods.
    let readPhase = do
          q <- sequence =<< readEvent hQuit
          t <- sequence =<< readEvent hTick
          return (q, t)

    -- Triggers events created using `TriggerEvent`
    let fireEventTriggerRefs ers rcb = do
          mes <- liftIO $
            for ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
              me <- readIORef er
              pure $ fmap (==> a) me
          a <- fire (catMaybes mes) rcb
          liftIO $ for_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
          pure a

    -- Exit callback, shut down with success.
    let exit = liftIO $ do
          putStrLn "Exiting..."
          exitSuccess

    -- Begin the event loop
    --
    -- Trigger the PostBuild event and react to any initial output (if any).
    maybePostBuildTrigger <- liftIO $ readIORef postBuildTriggerRef
    out <- case maybePostBuildTrigger of
      -- If nothing is listening, don't do anything.
      Nothing      -> return []
      Just trigger -> fire [trigger :=> Identity ()] readPhase

    -- Handle initial output and shutdown immediately if required
    handleOutput out >>= flip when exit . fst

    -- Push a tick to the Chan so we don't block indefinitely
    doTick

    -- Main loop.
    fix $ \loop -> do
      -- Read the event channel written to by `TriggerEvent` process the
      -- subsequent outputs.
      events <- liftIO $ readChan eventsChan
      out' <- fireEventTriggerRefs events readPhase
      (shouldShutdown, tickDone) <- handleOutput out'

      -- Try to keep GC latency low.
      liftIO performMinorGC

      when tickDone $ do
        scene' <- sample scene
        -- Render UI and scene
        render scene' ui
        -- Swap buffers. Also polls the window for events which will trigger
        -- any GLFW callbacks.
        swapWindowBuffers window
        -- Trigger all the input events including dispatching inputs to
        -- the right UI elements.
        processInputEvents
        -- Finally trigger a tick after all events from the last frame have
        -- been triggered
        doTick

      windowClose <- fmap (fromMaybe False) . windowShouldClose $ window

      if shouldShutdown || windowClose
        then exit
        else loop

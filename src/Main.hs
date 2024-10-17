module Main (
  main
) where

import Control.Concurrent
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Dependent.Sum
import Data.Foldable
import Data.Maybe
import Data.IORef
import Data.Time.Clock
import Data.Traversable
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
    (window, windowSize, render, elementIdTexture) <-
      Graphics.initialise name

    -- Create post build event.
    (ePostBuild, postBuildTriggerRef) <- newEventWithTriggerRef

    rec
      (eTick, tickTrigger, uiEvents) <-
        flip runTriggerEventT eventsChan $ do
          -- Dispatch inputs to the right UI element by sampling the element ID
          -- map texture at the mouse position / focus.
          --
          -- Unhandled inputs are dispatched to the UI events.
          (eKey, keyTrigger) <- newTriggerEvent
          _ <- lift . setKeyCallback window . Just $
                 \k _ ks _ -> keyTrigger (k, ks)

          (eCursorPos, cursorPosTrigger) <- newTriggerEvent
          _ <- lift . setCursorPosCallback window . Just $
                 \x y -> cursorPosTrigger . P . V2 (round x) $ round y

          (eMouseButton, mouseButtonTrigger) <- newTriggerEvent
          _ <- lift . setMouseButtonCallback window . Just $
                 \b s _ -> mouseButtonTrigger (b, s)

          -- Create a tick event so the host can trigger a tick after each frame
          -- render.
          (eTick', tickTrigger') <- newTriggerEvent

          let uiEvents' = UIEvents eCursorPos eKey eMouseButton

          return (eTick', tickTrigger', uiEvents')

      let env = Env eTick windowSize

      -- Create the network.
      (((eQuit, scene), ui, uiTriggers), FireCommand fire) <-
          hostPerformEventT
          . flip runPostBuildT ePostBuild
          . flip runTriggerEventT eventsChan
          . flip runReaderT env
          . runUiBuilderT uiEvents
          $ game

    -- Event handles - for reading current `Event` values, which may or may not
    -- have been produced since the last frame.
    hQuit <- subscribeEvent eQuit
    --hScene <- subscribeEvent eScene

    -- Exit callback, shuts down .
    let exit = liftIO $ do
          putStrLn "Exiting..."
          exitSuccess

    -- The game tick which is synchronised with each render.
    timeRef <- liftIO $ newIORef =<< getCurrentTime

    let doTick = liftIO $ do
          now' <- getCurrentTime
          deltaT <- fmap (realToFrac . diffUTCTime now') . readIORef $ timeRef
          tickTrigger deltaT
          writeIORef timeRef now'

        doRender scene' = do
          render scene' ui
          doTick

    -- Processes event outputs and finds out if we should shutdown.
    -- There will be a list of outputs, one for each event triggered in the
    -- last frame so we have to process them all accordingly.
    let handleOutput outs = do
          {-
          mapM_ doRender . listToMaybe . mapMaybe snd $ outs
          -- Any quit event should cause the application to exit.
          return . any (isJust . fst) $ outs
          -}
          return . any isJust $ outs

    -- Reads output event handles and sequences any actions by PerformEvent
    -- methods.
    let readPhase = do
          {-
          q <- sequence =<< readEvent hQuit
          s <- sequence =<< readEvent hScene
          return (q, s)
          -}
          sequence =<< readEvent hQuit

    -- Triggers events created using `TriggerEvent`
    let fireEventTriggerRefs ers rcb = do
          mes <- liftIO $
            for ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
              me <- readIORef er
              pure $ fmap (==> a) me
          a <- fire (catMaybes mes) rcb
          liftIO $ for_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
          pure a

    -- Trigger the PostBuild event and react to any initial output (if any).
    maybePostBuildTrigger <- liftIO $ readIORef postBuildTriggerRef
    out <- case maybePostBuildTrigger of
      -- If nothing is listening, don't do anything.
      Nothing      -> return []
      Just trigger -> fire [trigger :=> Identity ()] readPhase

    -- Handle initial output and shutdown immediately if required
    handleOutput out >>= flip when exit

    -- Push a tick to the Chan so we don't block indefinitely
    doTick

    -- Main loop.
    fix $ \loop -> do
      -- Read the event channel written to by `TriggerEvent` process the
      -- subsequent outputs.
      events <- liftIO $ readChan eventsChan
      out' <- fireEventTriggerRefs events readPhase
      shouldShutdown <- handleOutput out'

      -- Try to keep GC latency low.
      liftIO performMinorGC

      doRender =<< sample scene

      if shouldShutdown
        then exit
        else loop

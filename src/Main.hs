module Main (
  main
) where

import Control.Concurrent
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Reader
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

import App.Env
import App.Game

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

  -- NB: This first dollar is to keep the type checker happy, otherwise the
  -- Rank-2 type parameter `os` escapes.
  runSpiderHost $ runContextT defaultHandleConfig $ do
    (env, renderScene, windowShouldClose', tickTrigger) <-
      flip runTriggerEventT eventsChan . initialise $ name

    -- Create post build event.
    (ePostBuild, postBuildTriggerRef) <- lift newEventWithTriggerRef

    -- Create the network.
    (Output{..}, FireCommand fire) <- lift . hostPerformEventT
      . flip runPostBuildT ePostBuild
      . flip runTriggerEventT eventsChan
      . flip runReaderT env
      $ game

    -- Event handles - for reading current `Event` values, which may or may not
    -- have been produced since the last frame.
    hQuit <- lift . subscribeEvent $ outputQuit
    hScene <- lift . subscribeEvent $ outputScene

    -- Exit callback which cleans up and shuts down successfully.
    let exit = do
          putStrLn "Exiting..."
          exitSuccess

    -- The game tick which is synchronised with each render.
    timeRef <- liftIO $ newIORef =<< getCurrentTime

    let doTick = liftIO $ do
          now' <- getCurrentTime
          deltaT <- fmap (realToFrac . diffUTCTime now') . readIORef $ timeRef
          tickTrigger deltaT
          writeIORef timeRef now'

    let doRender scene = do
          renderScene scene
          -- Call the tick trigger after we render so we know how long the last
          -- frame took and so there's an event in the chan.
          doTick

    -- Processes event outputs. There will be a list of outputs, one for each
    -- event triggered so we have to process them all accordingly.
    let handleOutput outs = do
          -- Any quit event should cause the application to exit.
          mapM_ (const $ liftIO exit) . mapMaybe fst $ outs
          -- Render the latest Picture output.
          mapM_ doRender . listToMaybe . mapMaybe snd . reverse $ outs

    -- Reads output event handles and sequences any actions by PerformEvent
    -- methods.
    let readPhase = do
          quit <- sequence =<< readEvent hQuit
          scene <- sequence =<< readEvent hScene
          return (quit, scene)

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
      Just trigger -> lift $ fire [trigger :=> Identity ()] readPhase
    handleOutput out

    -- Call the tick trigger once so there's an event in the channel and we
    -- don't block on the first read.
    doTick

    -- Main loop.
    fix $ \loop -> do
      -- Read the event channel written to by `TriggerEvent` process the
      -- subsequent outputs.
      events <- liftIO $ readChan eventsChan
      outs <- lift . fireEventTriggerRefs events $ readPhase
      handleOutput outs

      -- Exit if the window close button was pressed.
      closeRequested <- windowShouldClose'
      liftIO . when (closeRequested == Just True) $ exit

      -- Run the garbage collector every frame in an attempt to keep the major
      -- GC latency low.
      liftIO performMinorGC

      loop

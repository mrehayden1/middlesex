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

import App.Env

name :: String
name = "Middlesex"

version :: String
version = "0.1.0"

data Output t = Output {
    outputQuit :: Event t (),
    outputScene :: Event t (),
    outputTickTrigger :: UTCTime -> IO ()
  }

start :: forall os t m. App os t m => Event t () -> m (Output t)
start eKey = do
  -- Create a tick trigger event so the Reflex host can tick after each time it
  -- renders a frame.
  (eTick, tickTrigger) <- newTriggerEvent

  return $ Output {
      outputQuit = void eKey,
      outputScene = void eTick,
      outputTickTrigger = tickTrigger
    }

main :: IO ()
main = do
  putStrLn   ""
  putStrLn $ name ++ " — version " ++ version ++ " ..."
  putStrLn   ""
  putStrLn   "Copyright Categorical Industries 2024, All Rights Reserved."
  putStrLn   ""

  putStrLn "Starting..."

  eventsChan <- newChan

  -- NB: This first dollar is to keep the type checker happy (without
  -- ImpredicativeTypes), otherwise the Rank-2 type parameter `os` escapes.
  runSpiderHost $ runContextT defaultHandleConfig $ do
    env@Env{..} <- initialise name

    shader <- compileShader $ do
      primitiveStream <- toPrimitiveStream id
      let primitiveStream2 = flip fmap primitiveStream $ \(pos, clr) ->
            (pos + V4 0.3 0.3 0 0, clr * 2)
      let primitiveStream3 = primitiveStream `mappend` primitiveStream2
      fragmentStream <- flip rasterize primitiveStream3 . const $
         (FrontAndBack,
          ViewPort (V2 0 0) (V2 windowWidth windowHeight),
          DepthRange 0 1
         )
      drawWindowColor
        (const (envWindow, ContextColorOption NoBlending (V3 True True True)))
        fragmentStream

    -- Create input events.
    (eKey, keyTrigger) <- lift . runTriggerEventT newTriggerEvent $ eventsChan
    _ <- setKeyCallback envWindow . Just $ \_ _ _ _ -> keyTrigger ()

    -- Create post build event.
    (ePostBuild, postBuildTriggerRef) <- lift newEventWithTriggerRef

    -- Create the network.
    (Output{..}, FireCommand fire) <- lift . hostPerformEventT
      . flip runPostBuildT ePostBuild
      . flip runTriggerEventT eventsChan
      . flip runReaderT env
      $ start eKey

    -- Event handles - for reading current `Event` values, which may or may not
    -- have been produced since the last frame.
    hQuit <- lift . subscribeEvent $ outputQuit
    hScene <- lift . subscribeEvent $ outputScene

    -- Exit callback which cleans up and shuts down successfully.
    let exit = do
          putStrLn "Exiting..."
          exitSuccess

    let doRender = render $ do
          clearWindowColor envWindow (V3 0 0 0)
          vertexArray <- newVertexArray envVertexBuffer
          let primitiveArray = toPrimitiveArray TriangleStrip vertexArray
          shader primitiveArray

    -- Processes event outputs. There will be a list of outputs, one for each
    -- event triggered so we have to process them all accordingly.
    let handleOutput outs = do
          -- Any quit event should cause the application to exit.
          mapM_ (const $ liftIO exit) . mapMaybe fst $ outs
          -- Render the latest Picture output.
          mapM_ (const doRender) . listToMaybe . mapMaybe snd . reverse $ outs
          -- Swap buffers. Also polls the window for events which will collect
          -- any `TriggerEvent` events dispatched by GLFW callbacks.
          swapWindowBuffers envWindow

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

    liftIO $ outputTickTrigger =<< getCurrentTime

    -- Main loop.
    fix $ \loop -> do
      -- Read the event channel written to by `TriggerEvent` process the
      -- subsequent outputs.
      events <- liftIO $ readChan eventsChan
      outs <- lift . fireEventTriggerRefs events $ readPhase
      handleOutput outs

      -- Call the tick trigger after we render so we know how long the last
      -- frame took and so there's an event in the chan.
      liftIO $ outputTickTrigger =<< getCurrentTime

      closeRequested <- windowShouldClose envWindow
      unless (closeRequested == Just True) loop

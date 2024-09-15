{-# OPTIONS_GHC -Wno-orphans #-}
module App.Env (
  module Graphics,

  App,
  Env(..),

  initialise
) where

import Control.Monad.Ref
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader
import Reflex
import Reflex.Host.Class

import App.Graphics hiding (initialise)
import qualified App.Graphics as Graphics

type App os t m = (MonadIO m, MonadIO (Performable m), MonadFix m,
  MonadHold t m, Adjustable t m, NotReady t m, PerformEvent t m,
  PostBuild t m, TriggerEvent t m, MonadReader (Env os t) m)

data Env os t = Env {
     envEKey :: Event t (),
     envETick :: Event t ()
  }

initialise :: (m ~ SpiderHost Global, t ~ SpiderTimeline Global)
 => String
 -> TriggerEventT t
      (ContextT Handle os m)
      (Env os t, ContextT Handle os m (), ContextT Handle os m (Maybe Bool), () -> IO ())
initialise name = do
  (window, renderScene) <- lift . Graphics.initialise $ name

  -- Create input events.
  (eKey, keyTrigger) <- newTriggerEvent
  _ <- lift . setKeyCallback window . Just $ \_ _ _ _ -> keyTrigger ()

  -- Create a tick event so the host can trigger a tick after each frame
  -- render.
  (eTick, tickTrigger) <- newTriggerEvent

  let env = Env eKey eTick

  let renderScene' = do
        renderScene
        -- Swap buffers. Also polls the window for events which will collect
        -- any `TriggerEvent` events dispatched by GLFW callbacks.
        swapWindowBuffers window

  let windowShouldClose' = windowShouldClose window

  return (env, renderScene', windowShouldClose', tickTrigger)

instance TriggerEvent t m => TriggerEvent t (ContextT ctx os m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = lift newTriggerEvent
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance MonadRef m => MonadRef (ContextT ctx os m) where
  type Ref (ContextT ctx os m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (ContextT handle os m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer

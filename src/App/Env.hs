{-# OPTIONS_GHC -Wno-orphans #-}
module App.Env (
  module Graphics,

  App,
  Env(..),

  Key(..),
  KeyState(..),

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
     eCursorPos :: Event t (Int, Int),
     eKey :: Event t (Key, KeyState),
     eMouseButton :: Event t (MouseButton, MouseButtonState),
     eTick :: Event t DeltaT,
     windowSize :: (Int, Int)
  }

type DeltaT = Float

initialise :: (m ~ SpiderHost Global, t ~ SpiderTimeline Global)
 => String
 -> TriggerEventT t
      (ContextT Handle os m)
      (Env os t,
       Scene -> ContextT Handle os m (),
       ContextT Handle os m (Maybe Bool),
       DeltaT -> IO ())
initialise name = do
  (window, renderScene, windowSize) <- lift . Graphics.initialise $ name

  -- Create input events.
  (eKey, keyTrigger) <- newTriggerEvent
  _ <- lift . setKeyCallback window . Just $ \k _ ks _ -> keyTrigger (k, ks)

  (eCursorPos, cursorPosTrigger) <- newTriggerEvent
  _ <- lift . setCursorPosCallback window . Just $
         \x y -> cursorPosTrigger (round x, round y)

  (eMouseButton, mouseButtonTrigger) <- newTriggerEvent
  _ <- lift . setMouseButtonCallback window . Just $
         \b s _ -> mouseButtonTrigger (b, s)

  -- Create a tick event so the host can trigger a tick after each frame
  -- render.
  (eTick, tickTrigger) <- newTriggerEvent

  let env = Env eCursorPos eKey eMouseButton eTick windowSize

  let renderScene' scene = do
        renderScene scene
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

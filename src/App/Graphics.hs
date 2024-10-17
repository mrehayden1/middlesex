{-# OPTIONS_GHC -Wno-orphans #-}
module App.Graphics (
  module App.Graphics.Camera,
  module App.Graphics.Projection,

  module Linear,
  module Linear.Affine,

  Scene(..),
  UIElem,

  initialise,

  ContextT(..),
  runContextT,

  ContextHandler,

  Window',
  WindowSize,

  Handle,
  defaultHandleConfig,

  swapWindowBuffers,

  windowShouldClose,

  setKeyCallback,
  Key(..),
  KeyState(..),
  ModifierKeys(..),

  setCursorPosCallback,

  setMouseButtonCallback,
  MouseButton(..),
  MouseButtonState(..)
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Exception
import Control.Monad.Ref
import Data.Maybe
import Graphics.GPipe
import Graphics.GPipe.Context.GLFW
import qualified Graphics.UI.GLFW as GLFW
import Linear
import Linear.Affine
import Reflex
import Reflex.Host.Class

import App.Graphics.Camera
import App.Graphics.Projection
import App.Graphics.Scene as Scene
import App.Graphics.UI as UI
import App.Graphics.Window

fullscreen :: Bool
--fullscreen = True
fullscreen = False

windowHeight, windowWidth :: Maybe Int
windowHeight = Just 1080
windowWidth = Just 1920

--windowHeight = Just 1440
--windowWidth = Just 2560

--windowHeight = Just 1800
--windowWidth = Just 3200

--windowHeight = Nothing
--windowWidth = Nothing

-- When no window size can be determined, either from the setting above or the
-- primary monitor's current video mode, we default to these values.
windowHeightFallback, windowWidthFallback :: Int
windowHeightFallback = 1080
windowWidthFallback = 1920

initialise :: (ctx ~ Handle, MonadIO m, MonadException m)
  => String
  -> ContextT ctx os m (
         Window' os,
         WindowSize,
         Scene -> [UIElem] -> ContextT ctx os m (),
         ElemIdTexture os
       )
initialise name = do
  mMonitor <- liftIO GLFW.getPrimaryMonitor
  mVideoMode <- liftIO . fmap join . mapM GLFW.getVideoMode $ mMonitor

  let windowHeight' = fromMaybe windowHeightFallback $
                        windowHeight <|> fmap GLFW.videoModeHeight mVideoMode
      windowWidth'  = fromMaybe windowWidthFallback $
                        windowWidth <|> fmap GLFW.videoModeWidth mVideoMode

      windowSize    = (windowWidth', windowHeight')

  let windowConfig = WindowConfig {
          configHeight = windowHeight',
          configHints = [GLFW.WindowHint'Samples (Just 8)],
          -- Setting a monitor runs the app in fullscreen.
          configMonitor = if fullscreen then mMonitor else Nothing,
          configSwapInterval = Just 0,
          configTitle = name,
          configWidth = windowWidth'
        }

  window <- newWindow (WindowFormatColorDepth SRGB8A8 Depth32) windowConfig

  renderScene <- Scene.createRenderer window windowSize

  (renderUi, uiElemIdTexture) <- UI.createRenderer window windowSize

  let doRender scene ui = do
        -- Clear the colour and depth back buffers
        render $ do
          clearWindowColor window $ V4 0 0 0 1
          clearWindowDepth window 1
        -- Render scene
        renderScene scene
        -- Clear the depth back buffer and render UI
        render $ clearWindowDepth window 1
        renderUi . UI . UIColumn $ ui
        -- Swap buffers. Also polls the window for events which will collect
        -- any `TriggerEvent` events dispatched by GLFW callbacks.
        swapWindowBuffers window

  return (window, windowSize, doRender, uiElemIdTexture)

{-
 - Orphan ContextT instances
 -}
instance MonadRef m => MonadRef (ContextT ctx os m) where
  type Ref (ContextT ctx os m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r

instance MonadSubscribeEvent t m
  => MonadSubscribeEvent t (ContextT ctx os m) where
    subscribeEvent = lift . subscribeEvent

instance MonadReflexHost t m => MonadReflexHost t (ContextT ctx os m) where
  type ReadPhase (ContextT ctx os m) = ReadPhase m
  fireEventsAndRead dm a = lift $ fireEventsAndRead dm a
  {-# INLINE fireEventsAndRead #-}
  runHostFrame = lift . runHostFrame
  {-# INLINE runHostFrame #-}

instance MonadReflexCreateTrigger t m
  => MonadReflexCreateTrigger t (ContextT handle os m) where
    newEventWithTrigger = lift . newEventWithTrigger
    newFanEventWithTrigger initializer = lift
                                          $ newFanEventWithTrigger initializer

instance TriggerEvent t m => TriggerEvent t (ContextT ctx os m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = lift newTriggerEvent
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift .
    newEventWithLazyTriggerWithOnComplete

instance MonadSample t m => MonadSample t (ContextT ctx os m) where
  sample = lift . sample

instance (MonadHold t m) => MonadHold t (ContextT ctx os m) where
  hold a0 = lift . hold a0
  holdDyn a0 = lift . holdDyn a0
  holdIncremental a0 = lift . holdIncremental a0
  buildDynamic a0 = lift . buildDynamic a0
  headE = lift . headE
  now = lift now

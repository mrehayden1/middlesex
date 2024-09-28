module App.Graphics (
  module App.Graphics.Camera,
  module App.Graphics.Projection,

  module Linear,

  Scene(..),

  initialise,

  ContextT,
  runContextT,

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
import Control.Monad.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Maybe
import Graphics.GPipe
import Graphics.GPipe.Context.GLFW
import qualified Graphics.UI.GLFW as GLFW
import Linear

import App.Graphics.Camera
import App.Graphics.Projection
import App.Graphics.Scene
import qualified App.Graphics.Text as Text
import App.Graphics.Window

defaultDpi :: Int
defaultDpi = 240

fullscreen :: Bool
--fullscreen = True
fullscreen = False

windowHeight, windowWidth :: Maybe Int
--windowHeight = Just 1080
--windowWidth = Just 1920

windowHeight = Just 1440
windowWidth = Just 2560

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
         Scene -> ContextT ctx os m ()
       )
initialise name = do
  mMonitor <- liftIO GLFW.getPrimaryMonitor
  mVideoMode <- liftIO . fmap join . mapM GLFW.getVideoMode $ mMonitor

  -- Calculate the DPI
  dpi <- liftIO . fmap (fromMaybe defaultDpi) . runMaybeT $ getDpi

  liftIO . print $ dpi

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

  window <- newWindow (WindowFormatColor RGBA8) windowConfig

  renderScene <- createSceneRenderer window windowSize

  renderText <- Text.initialise window windowSize

  let render' scene = do
        renderScene scene
        -- Render text
        renderText 0 (V4 0 0 0 1) 100 (V2 1024 768) "Hello, Worldy!"

  return (window, windowSize, render')

getDpi :: MaybeT IO Int
getDpi = do
  monitor <- MaybeT GLFW.getPrimaryMonitor
  widthMm <- liftIO . GLFW.getMonitorPhysicalSize $ monitor
  videoMode <- MaybeT . GLFW.getVideoMode $ monitor
  let widthIn = mmToIn . fst $ widthMm
      widthPx = fromIntegral . GLFW.videoModeWidth $ videoMode
  return . round $ widthPx / widthIn
 where
  mmToIn :: Int -> Float
  mmToIn = (/ 25.4) . fromIntegral

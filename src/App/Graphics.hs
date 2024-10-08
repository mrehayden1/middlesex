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
import Data.Maybe
import Graphics.GPipe
import Graphics.GPipe.Context.GLFW
import qualified Graphics.UI.GLFW as GLFW
import Linear

import App.Graphics.Camera
import App.Graphics.Projection
import App.Graphics.Scene as Scene
import App.Graphics.UI as UI
import App.Graphics.Window

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

  renderUi <- UI.createRenderer window windowSize

  font <- loadFont

  let button = UIButton
        . uiText font typefaceIxLabel 160 (V4 (248/255) (235/255) (222/255) 1)

  let render' scene = do
        -- Clear the colour and depth buffers
        render $ clearWindowColor window 0
        render $ clearWindowDepth window 0
        -- Render scene
        renderScene scene
        -- Clear depth buffer and render UI
        render $ clearWindowDepth window 0
        renderUi
          . UI
          . UICard
          . UILayoutColumn $ [
              button "Start",
              button "Options",
              button "Exit"
            ]

  return (window, windowSize, render')

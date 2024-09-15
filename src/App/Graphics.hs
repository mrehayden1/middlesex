module App.Graphics (
  initialise,

  ContextT,
  runContextT,

  Handle,
  defaultHandleConfig,

  swapWindowBuffers,
  setKeyCallback,
  windowShouldClose,

) where

import Control.Monad.Exception
import Control.Monad.IO.Class
import Graphics.GPipe hiding (Render, Shader, render)
import qualified Graphics.GPipe as GPipe
import Graphics.GPipe.Context.GLFW
import qualified Graphics.UI.GLFW as GLFW

import App.Graphics.Env
import App.Graphics.Text hiding (Renderer, initialise)
import qualified App.Graphics.Text as Text

type Shader os = CompiledShader os (PrimitiveArray Triangles (B4 Float, B3 Float))

type Renderer ctx os m = ContextT ctx os m ()

windowHeight, windowWidth :: Int
windowHeight = 1080
windowWidth = 1920
--windowHeight = 1440
--windowWidth = 2560

windowSizes :: [(Int, Int)]
windowSizes = [
    (1920, 1080),
    (2560, 1440),
    (3840, 2160),
    (7680, 4320)
  ]

initialise :: (ctx ~ Handle, MonadIO m, MonadException m)
  => String
  -> ContextT ctx os m (Window' os, Renderer ctx os m)
initialise name = do
  monitor <- liftIO GLFW.getPrimaryMonitor
  -- Print current and available video modes.
  --liftIO $ mapM_ (print <=< GLFW.getVideoModes) monitor
  --liftIO $ mapM_ (print <=< GLFW.getVideoMode) monitor
  let windowConfig = WindowConfig {
          configHeight = windowHeight,
          configHints = [],
          -- Setting a monitor runs the app in fullscreen.
          --configMonitor = monitor,
          configMonitor = Nothing,
          configSwapInterval = Just 0,
          configTitle = name,
          configWidth = windowWidth
        }

  window <- newWindow (WindowFormatColor RGB8) windowConfig

  renderText <- Text.initialise window

  let render = do
        GPipe.render $
          clearWindowColor window $ V3 0 0 0
        renderText 0 1 (V2 960 540) "Hello, World!"

  return (window, render)

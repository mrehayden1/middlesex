module App.Env (
  module App.Graphics,

  App,
  Env(..),

  windowHeight,
  windowWidth,

  initialise
) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import qualified Graphics.UI.GLFW as GLFW
import Reflex

import App.Graphics hiding (initialise)
import qualified App.Graphics as Graphics

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

type App os t m = (MonadIO m, MonadIO (Performable m),
  MonadFix m, MonadHold t m, Adjustable t m, NotReady t m, PerformEvent t m,
  PostBuild t m, TriggerEvent t m)

data Env os = Env {
    envVertexBuffer :: Buffer os (B4 Float, B3 Float),
    envWindow :: Window os RGBFloat ()
  }

initialise :: MonadIO m => String -> ContextT Handle os m (Env os)
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
  Env
    <$> Graphics.initialise
    <*> newWindow (WindowFormatColor RGB8) windowConfig

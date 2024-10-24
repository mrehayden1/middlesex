module App.Class (
  App,

  Env(..),
  DeltaT
) where

import Control.Monad.Reader
import Reflex
import Reflex.Host.Class

import App.UI

type App os t m = (
    Adjustable t m,
    MonadFix m,
    MonadIO m,
    MonadIO (Performable m),
    MonadHold t m,
    MonadReader (Env os t) m,
    PerformEvent t m,
    PostBuild t m,
    ReflexHost t,
    TriggerEvent t m,
    UIBuilder t os m
  )

data Env os t = Env {
     eTick :: Event t DeltaT,
     windowSize :: (Int, Int)
  }

type DeltaT = Float

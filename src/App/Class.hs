module App.Class (
  App,

  Env(..),
  DeltaT
) where

import Control.Monad.Ref
import Control.Monad.Reader
import Data.IORef
import Linear.Affine
import Reflex
import Reflex.Host.Class

import App.UI

type App os t m = (
    ReflexHost t,
    MonadIO m,
    MonadIO (Performable m),
    MonadFix m,
    PostBuild t m,
    MonadHold t m,
    PerformEvent t m,
    TriggerEvent t m,
    MonadReader (Env os t) m,
    UIBuilder t m
  )

data Env os t = Env {
     eTick :: Event t DeltaT,
     windowSize :: (Int, Int)
  }

type DeltaT = Float

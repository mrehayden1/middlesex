module App.Graphics.Render (
  Render
) where

import Control.Monad.Reader
import Graphics.GPipe hiding (Render)
import Graphics.GPipe.Context.GLFW

type Render os e m a = ReaderT e (ContextT Handle os m) a

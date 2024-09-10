module App.Graphics (
  module Graphics.GPipe,
  module Graphics.GPipe.Context.GLFW,

  initialise
) where

import Control.Monad.IO.Class
import Graphics.GPipe
import Graphics.GPipe.Context.GLFW

initialise :: (ContextHandler ctx, MonadIO m)
  => ContextT ctx os m (Buffer os (B4 Float, B3 Float))
initialise = do
  --                        x      y   z   w       r   g   b
  let mapQuad = [ (V4   0.333    0.5   0   1, V3   1   0   0)
                , (V4 (-0.333)   0.5   0   1, V3   0   1   0)
                , (V4   0.333  (-0.5)  0   1, V3   1   0   1)
                , (V4 (-0.333) (-0.5)  0   1, V3   0   0   1)
                ]

  vertexBuffer <- newBuffer . length $ mapQuad
  writeBuffer vertexBuffer 0 mapQuad
  return vertexBuffer

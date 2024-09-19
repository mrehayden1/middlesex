module App.Matrix (
  eulerDirection,
  rotateX,
  rotateY,
  rotateZ
) where

import Linear

eulerDirection :: Floating a => a -> a -> V3 a
eulerDirection pitch yaw =
  let x = cos yaw * cos pitch
      y = sin pitch
      z = negate $ sin yaw * cos pitch
  in signorm $ V3 x y z

rotateX :: Floating a => a -> M44 a
rotateX t =
  V4 (V4 1       0        0 0)
     (V4 0 (cos t) (-sin t) 0)
     (V4 0 (sin t) ( cos t) 0)
     (V4 0       0        0 1)

rotateY :: Floating a => a -> M44 a
rotateY t =
  V4 (V4 ( cos t) 0 (sin t) 0)
     (V4        0 1       0 0)
     (V4 (-sin t) 0 (cos t) 0)
     (V4        0 0       0 1)

rotateZ :: Floating a => a -> M44 a
rotateZ t =
  V4 (V4 (cos t) (-sin t) 0 0)
     (V4 (sin t) ( cos t) 0 0)
     (V4       0        0 1 0)
     (V4       0        0 0 1)

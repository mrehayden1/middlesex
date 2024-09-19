module App.Graphics.Projection (
  projection,
  inverseProjection
) where

import Linear

fov :: Floating a => a
fov = 20

nearPlane, farPlane :: Floating a => a
nearPlane = 0.001
farPlane = 10000

-- The viewport projection matrix
projection :: forall a. Floating a => Int -> Int -> M44 a
projection viewportWidth viewportHeight =
  let vw = realToFrac viewportWidth
      vh = realToFrac viewportHeight
  in perspective (fov * pi / 180) (vw / vh) nearPlane farPlane

-- The inverse viewport projection matrix
inverseProjection :: Floating a => Int -> Int -> M44 a
inverseProjection viewportWidth viewportHeight =
  let vw = realToFrac viewportWidth
      vh = realToFrac viewportHeight
  in inversePerspective (fov * pi / 180) (vw / vh) nearPlane farPlane

module App.Matrix (
  Camera(..),
  toViewMatrix,
  toInverseViewMatrix,

  rotateX,
  rotateY,
  rotateZ
) where

import Control.Lens
import Linear

data Camera a = Camera {
  camPitch :: !a,    -- Anticlockwise looking down x
  camPos :: !(V3 a),
  camYaw :: !a       -- Anticlockwise looking down y
} deriving (Show)

-- Directional unit vector of the camera given pitch and yaw
camDirection :: Floating a => Camera a -> V3 a
camDirection Camera{..} = eulerDirection camPitch camYaw

camRight :: Floating a => Camera a -> V3 a
camRight Camera{..} = V3 (sin camYaw) 0 (cos camYaw)

toViewMatrix :: (Floating a) => Camera a -> M44 a
toViewMatrix cam@Camera{..} =
  let dir    = camDirection cam
      -- the 'centre' to which the camera is looking
      centre = camPos + dir
      -- no camera roll so the camera is always on the x-z plane
      up     = camRight cam `cross` dir -- camera's up
  in lookAt' camPos centre up

toInverseViewMatrix :: Floating a => Camera a -> M44 a
toInverseViewMatrix = inv44 . toViewMatrix

lookAt' :: Floating a => V3 a -> V3 a -> V3 a -> M44 a
lookAt' eye center up =
  V4 (V4 (xa^._x)  (xa^._y)  (xa^._z)  xd)
     (V4 (ya^._x)  (ya^._y)  (ya^._z)  yd)
     (V4 (-za^._x) (-za^._y) (-za^._z) zd)
     (V4 0         0         0          1)
  where za = signorm $ center - eye
        xa = signorm $ cross za up
        ya = cross xa za
        xd = -dot xa eye
        yd = -dot ya eye
        zd = dot za eye

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

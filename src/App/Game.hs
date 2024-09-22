module App.Game (
  Output(..),

  game
) where

import Control.Monad.Reader
import Data.Bool
import Linear
import Reflex

import App.Env

data Output t = Output {
    outputQuit :: Event t (),
    outputScene :: Event t (Camera Float)
  }

game :: forall os t m. App os t m => m (Output t)
game = do
  Env{..} <- ask

  let windowRight  = fst windowSize - 1
      windowBottom = snd windowSize - 1

  keyUp    <- keyHeld Key'Up    eKey
  keyDown  <- keyHeld Key'Down  eKey
  keyLeft  <- keyHeld Key'Left  eKey
  keyRight <- keyHeld Key'Right eKey

  cursorWindowLeft   <- holdDyn False . fmap ((<= 0) . fst)
                          $ eCursorPos
  cursorWindowRight  <- holdDyn False . fmap ((>= windowRight) . fst)
                          $ eCursorPos
  cursorWindowTop    <- holdDyn False . fmap ((<= 0) . snd)
                          $ eCursorPos
  cursorWindowBottom <- holdDyn False . fmap ((>= windowBottom) . snd)
                          $ eCursorPos

  let camMoveUp    = (||) <$> keyUp    <*> cursorWindowTop
      camMoveDown  = (||) <$> keyDown  <*> cursorWindowBottom
      camMoveLeft  = (||) <$> keyLeft  <*> cursorWindowLeft
      camMoveRight = (||) <$> keyRight <*> cursorWindowRight

      camVelUp    = bool 0 (-speed) <$> camMoveUp
      camVelDown  = bool 0   speed  <$> camMoveDown
      camVelLeft  = bool 0 (-speed) <$> camMoveLeft
      camVelRight = bool 0   speed  <$> camMoveRight

      camVelX = (+) <$> camVelLeft <*> camVelRight
      camVelY = (+) <$> camVelUp   <*> camVelDown
      camVel  = V2  <$> camVelX    <*> camVelY

      camDisp = (^*) <$> current camVel <@> eTick

  cameraPos <- foldDyn (+) 0 camDisp

  let camera = flip fmap cameraPos $ \(V2 camX camZ) ->
                  Camera {
                    camPitch = -(3 * pi) / 8,
                    camPos   = V3 camX 0 camZ + V3 0 25 10,
                    camYaw   = pi / 2
                  }

  cursorPos <- holdDyn (0, 0) eCursorPos

  let mapClickPos = fmap (uncurry (uncurry screenMapCoords windowSize))
        . tag ((,) <$> current camera <*> current cursorPos)
        . flip ffilter eMouseButton
        $ \(b, s) -> b == MouseButton'1 && s == MouseButtonState'Pressed

  return $ Output {
      outputQuit = void . ffilter ((== Key'Escape) . fst) $ eKey,
      outputScene = updated camera
    }
 where
  -- in tile widths
  speed :: Float
  speed = 2.5


keyHeld :: App os t m => Key -> Event t (Key, KeyState) -> m (Dynamic t Bool)
keyHeld k =
  holdDyn False . fmap ((== KeyState'Pressed) . snd) . ffilter (uncurry f)
 where
  f key state = key == k &&
                  (state == KeyState'Pressed || state == KeyState'Released)

-- Screen coordinates as co-ordinates on the plane y = 0
screenMapCoords :: Int -> Int -> Camera Float -> (Int, Int) -> V2 Float
screenMapCoords viewportWidth viewportHeight cam (cursorX, cursorY) =
  let projM' = inverseProjection viewportWidth viewportHeight
      viewM' = inv44 . toViewMatrix $ cam
      xNdc =   toNdc (realToFrac viewportWidth)  . realToFrac $ cursorX
      yNdc = -(toNdc (realToFrac viewportHeight) . realToFrac $ cursorY)
      V4 viewX viewY _ _ = projM' !* V4 xNdc yNdc (-1) 1
      V4 v1 v2 v3 _ = viewM' !* V4 viewX viewY (-1) 0
      V3 a b c = camPos cam
      -- Using the vector equation of the line `(x,y,z) = (a,b,c) + tv` we
      -- solve t for y = 0 and calculate x and z
      t = -b / v2
      x = a + t * v1
      z = c + t * v3
  in V2 x z
 where
  toNdc :: Float -> Float -> Float
  toNdc d a = 2 * (a - (d / 2)) / d

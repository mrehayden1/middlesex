module App (
  game
) where

import Control.Lens
import Control.Monad
import Control.Monad.Random
import Control.Monad.Reader
import Data.Bool
import Linear
import Linear.Affine
import Reflex

import App.Class
import App.Game.Board
import App.Graphics
import App.UI

game :: App os t m => m (Event t (), Behavior t Scene)
game = do
  Env{..} <- ask

  UIEvents{..} <- unhandledEvents

  exitE <- card $ do
    _ <- button "Start"
    _ <- button "Options"
    button "Exit"

  -- Map seed
  --let seed = 13011987
  seed <- liftIO randomIO
  let boardWidth  = 30
      boardHeight = 21
      board = genBoard boardWidth boardHeight seed

  let windowRight  = fst windowSize - 1
      windowBottom = snd windowSize - 1

  keyUp    <- keyHeld Key'Up    eKey
  keyDown  <- keyHeld Key'Down  eKey
  keyLeft  <- keyHeld Key'Left  eKey
  keyRight <- keyHeld Key'Right eKey

  cursorWindowLeft   <- holdDyn False . fmap ((<= 0) . (^. _x))
                          $ eCursorPos
  cursorWindowRight  <- holdDyn False . fmap ((>= windowRight) . (^. _x))
                          $ eCursorPos
  cursorWindowTop    <- holdDyn False . fmap ((<= 0) . (^. _y))
                          $ eCursorPos
  cursorWindowBottom <- holdDyn False . fmap ((>= windowBottom) . (^. _y))
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
                    camPos   = V3 camX 0 camZ + V3 0 50 10,
                    camYaw   = pi / 2
                  }

  cursorPos <- holdDyn (P $ V2 0 0) eCursorPos

  let mapClickE = fmap (uncurry (uncurry screenMapCoords windowSize))
        . tag ((,) <$> current camera <*> current cursorPos)
        . flip ffilter eMouseButton
        $ \(b, s) -> b == MouseButton'1 && s == MouseButtonState'Pressed

  tileIndexSelection <- holdDyn Nothing
    . fmap (Just . fromAxial . roundAxial . toAxial . (\(V2 x y) -> (x, y))
              . subtract (tileMapTranslation (boardWidth, boardHeight) (0, 0))
              . unP)
    $ mapClickE

  let eQuit = void . ffilter ((== Key'Escape) . fst) $ eKey
      scene = current $ Scene board <$> camera <*> tileIndexSelection

  return (eQuit, scene)

 where
  speed :: Float
  speed = 2.5


keyHeld :: App os t m => Key -> Event t (Key, KeyState) -> m (Dynamic t Bool)
keyHeld k =
  holdDyn False . fmap ((== KeyState'Pressed) . snd) . ffilter (uncurry f)
 where
  f key state = key == k &&
                  (state == KeyState'Pressed || state == KeyState'Released)

-- Screen coordinates to co-ordinates in the game map space (the world x/z
-- plane).
screenMapCoords :: Int -> Int -> Camera Float -> Point V2 Int -> Point V2 Float
screenMapCoords viewportWidth viewportHeight cam (P (V2 cursorX cursorY)) =
  let xNdc =   toNdc (realToFrac viewportWidth)  . realToFrac $ cursorX
      yNdc = -(toNdc (realToFrac viewportHeight) . realToFrac $ cursorY)
      -- Project the point on viewport back into view space.
      V4 viewX viewY _ _ = projM' !* V4 xNdc yNdc (-1) 1
      -- Take a unit vector pointing out the front of the camera (z = -1) and
      -- project it into world space.
      V4 v1 v2 v3 _ = viewM' !* V4 viewX viewY (-1) 0
      -- Camera position in world space.
      V3 a b c = camPos cam
      -- Using the vector equation of the line `(x,y,z) = (a,b,c) + tv` we
      -- solve t for y = 0 and calculate x and z
      t = -b / v2
      x = a + t * v1
      z = c + t * v3
  in P $ V2 x z
 where
  projM' = inverseProjection viewportWidth viewportHeight
  viewM' = inv44 . toViewMatrix $ cam

  -- Inverse viewport transformation
  toNdc :: Float -> Float -> Float
  toNdc dim x = 2 * (x - (dim / 2)) / dim

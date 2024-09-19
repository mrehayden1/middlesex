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
    outputScene :: Event t (V2 Float)
  }

keyHeld :: App os t m => Key -> Event t (Key, KeyState) -> m (Dynamic t Bool)
keyHeld k =
  holdDyn False . fmap ((== KeyState'Pressed) . snd) . ffilter (uncurry f)
 where
  f key state = key == k &&
                  (state == KeyState'Pressed || state == KeyState'Released)

game :: forall os t m. App os t m => m (Output t)
game = do
  Env{..} <- ask

  let windowRight  = fst windowSize - 1
      windowBottom = snd windowSize - 1

  keyUp    <- keyHeld Key'Up    eKey
  keyDown  <- keyHeld Key'Down  eKey
  keyLeft  <- keyHeld Key'Left  eKey
  keyRight <- keyHeld Key'Right eKey

  cursorLeft   <- holdDyn False . fmap ((== 0) . fst)            $ eCursorPos
  cursorRight  <- holdDyn False . fmap ((== windowRight) . fst)  $ eCursorPos
  cursorTop    <- holdDyn False . fmap ((== 0) . snd)            $ eCursorPos
  cursorBottom <- holdDyn False . fmap ((== windowBottom) . snd) $ eCursorPos

  let up    = (||) <$> keyUp    <*> cursorTop
      down  = (||) <$> keyDown  <*> cursorBottom
      left  = (||) <$> keyLeft  <*> cursorLeft
      right = (||) <$> keyRight <*> cursorRight

      velUp    = bool 0 (-speed) <$> up
      velDown  = bool 0   speed  <$> down
      velLeft  = bool 0 (-speed) <$> left
      velRight = bool 0   speed  <$> right

      velX = (+) <$> velLeft <*> velRight
      velY = (+) <$> velUp <*> velDown
      vel  = V2  <$> velX <*> velY

      disp = (^*) <$> current vel <@> eTick

  pos <- foldDyn (+) 0 disp

  return $ Output {
      outputQuit = void . ffilter ((== Key'Escape) . fst) $ eKey,
      outputScene = updated pos
    }
 where
  -- in tile widths
  speed :: Float
  speed = 2.5

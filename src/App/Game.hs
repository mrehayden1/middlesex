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

  up    <- pure . fmap (bool 0 (-1)) <=< keyHeld Key'Up    $ envEKey
  down  <- pure . fmap (bool 0   1 ) <=< keyHeld Key'Down  $ envEKey
  left  <- pure . fmap (bool 0 (-1)) <=< keyHeld Key'Left  $ envEKey
  right <- pure . fmap (bool 0   1 ) <=< keyHeld Key'Right $ envEKey

  let velX = (+) <$> left <*> right
      velY = (+) <$> up   <*> down
      vel  = V2 <$> velX <*> velY

      disp = (^*) <$> current vel <@> envETick

  pos <- foldDyn (+) 0 disp

  return $ Output {
      outputQuit = void . ffilter ((== Key'Escape) . fst) $ envEKey,
      outputScene = updated pos
    }

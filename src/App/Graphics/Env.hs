module App.Graphics.Env (
  Window'
) where

import Graphics.GPipe

type Window' os = Window os RGBFloat ()

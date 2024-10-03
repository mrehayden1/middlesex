module App.Graphics.Window (
  Window',
  WindowSize
) where

import Graphics.GPipe

type Window' os = Window os RGBAFloat Depth

type WindowSize = (Int, Int)

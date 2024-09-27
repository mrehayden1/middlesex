module App.Graphics.Board (
  makeModels,

  mapInstances
) where

import Control.Arrow
import Control.Monad.IO.Class
import Data.Map (Map)
import Graphics.GPipe

import App.Game.Board
import qualified App.Graphics.Board.Tile as Tile
import App.Graphics.Env
import App.Graphics.Texture
import App.Math.Matrix

mapTexturePath :: FilePath
mapTexturePath = "assets/textures/map.png"

makeModels :: (ContextHandler ctx, MonadIO m)
  => ContextT ctx os m (Model os, Map Tile (Model os))
makeModels = do
  -- Map data
  mapTexture <- fromPng mapTexturePath

  mapVertexBuffer :: Buffer os BVertex <- newBuffer . length $ mapVertices
  writeBuffer mapVertexBuffer 0 mapVertices

  let mapBaseColour = V4 1 1 1 1
      mapMaterial = Material mapTexture mapBaseColour

      mapModel = Model mapMaterial TriangleStrip mapVertexBuffer

  -- Tile data
  tileModels <- Tile.makeModels

  return (mapModel, tileModels)

-- Unit square map centered around the origin, which we'll transform later to
-- fit the board.
mapVertices :: [(V3 Float, V2 Float)]
mapVertices =
  fmap (first (\(V2 x z) -> V3 x 0 z)) [sw, se, nw, ne]
 where
  {-
  sw = (V2 ((-w / 2) - padding) ((-h / 2) - padding), V2 0 1)
  se = (V2 (( w / 2) + padding) ((-h / 2) - padding), V2 1 1)
  ne = (V2 (( w / 2) + padding) (( h / 2) + padding), V2 1 0)
  nw = (V2 ((-w / 2) - padding) (( h / 2) + padding), V2 0 0)
  -}
  --        x      y         u v
  sw = (V2 (-0.5) (-0.5), V2 0 1)
  se = (V2   0.5  (-0.5), V2 1 1)
  ne = (V2   0.5    0.5 , V2 1 0)
  nw = (V2 (-0.5)   0.5 , V2 0 0)

mapInstances :: Board -> [M44 Float]
mapInstances board =
  let w = sqrt 3 * fromIntegral nx
      h = (3 * fromIntegral ny) / 2 + (1 / 2)

      (nx, ny) = boardSize board

      padding = 1
  in pure . scale . V3 (w + padding * 2) 1 $ h + padding * 2

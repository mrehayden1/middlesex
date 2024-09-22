module App.Graphics.Board (
  makeModels
) where

import Control.Arrow
import Control.Monad.Random
import Data.Map (Map)
import Graphics.GPipe

import App.Game.Board
import qualified App.Graphics.Board.Tile as Tile
import App.Graphics.Env
import App.Graphics.Texture

mapTexturePath :: FilePath
mapTexturePath = "assets/textures/map.png"

makeModels :: (ContextHandler ctx, MonadIO m)
  => ContextT ctx os m (Model os, Map Tile (Model os, [M44 Float]))
makeModels = do
  -- Tile data
  let boardWidth  = 30
      boardHeight = 21
      board = flip evalRand (mkStdGen 13011987) . makeBoard boardWidth
                $ boardHeight

  -- Map data
  mapTexture <- fromPng mapTexturePath

  let mapVertices' = mapVertices board
  mapVertexBuffer :: Buffer os BVertex <- newBuffer . length $ mapVertices'
  writeBuffer mapVertexBuffer 0 mapVertices'

  let mapBaseColour = V4 1 1 1 1
      mapMaterial = Material mapTexture mapBaseColour

      mapModel = Model mapMaterial TriangleList mapVertexBuffer

  tileInstances <- Tile.makeModels board

  return (mapModel, tileInstances)


-- Make the geometry of a background for a hextile board with unit size tiles
-- and given padding.
mapVertices :: Board -> [(V3 Float, V2 Float)]
mapVertices board =
  fmap (first (\(V2 x z) -> V3 x 0 z)) [sw, se, nw, se, ne, nw]
 where
  sw = (V2 ((-w / 2) - padding) ((-h / 2) - padding), V2 0 1)
  se = (V2 (( w / 2) + padding) ((-h / 2) - padding), V2 1 1)
  ne = (V2 (( w / 2) + padding) (( h / 2) + padding), V2 1 0)
  nw = (V2 ((-w / 2) - padding) (( h / 2) + padding), V2 0 0)

  w  = sqrt 3 * fromIntegral nx
  h  = (3 * fromIntegral ny) / 2 + (1 / 2)

  (nx, ny) = boardSize board

  padding = 1

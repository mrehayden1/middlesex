module App.Graphics.Scene.Board.Tile (
  makeModels,

  instances
) where

import Control.Arrow
import Control.Monad.IO.Class
import Data.Map (Map)
import qualified Data.Map as M
import Graphics.GPipe

import App.Game.Board
import App.Graphics.Scene.Model
import App.Graphics.Texture
import App.Math.Matrix

textureDirectory :: FilePath
textureDirectory = "assets/textures/hextiles/"

textureFilePaths :: [(Tile, FilePath)]
textureFilePaths = [
    -- Land
    (Deadland1      , textureDirectory ++ "deadland-1.png"),
    (Deadland2      , textureDirectory ++ "deadland-2.png"),
    (Fields1        , textureDirectory ++ "fields-1.png"),
    (Fields2        , textureDirectory ++ "fields-2.png"),
    (Forest1        , textureDirectory ++ "forest-1.png"),
    (Forest2        , textureDirectory ++ "forest-2.png"),
    (ForestSparse1  , textureDirectory ++ "forest-sparse-1.png"),
    (ForestSparse2  , textureDirectory ++ "forest-sparse-2.png"),
    (Grass1         , textureDirectory ++ "grass-1.png"),
    (Grass2         , textureDirectory ++ "grass-2.png"),
    (GrassWindmills , textureDirectory ++ "grass-windmills.png"),
    (Hills          , textureDirectory ++ "hills-1.png"),
    (HillsHigh      , textureDirectory ++ "hills-2.png"),
    (HillsLow1      , textureDirectory ++ "hills-low-1.png"),
    (HillsLow2      , textureDirectory ++ "hills-low-2.png"),
    (HillsLowForest , textureDirectory ++ "hills-low-forest.png"),
    (Mountain       , textureDirectory ++ "mountain.png"),
    (Tor            , textureDirectory ++ "tor.png"),
    (TorLow         , textureDirectory ++ "tor-low.png"),
    -- Settlements
    (City           , textureDirectory ++ "settlement-city.png"),
    (CityMountain   , textureDirectory ++ "settlement-city-mountain.png"),
    (TownHill       , textureDirectory ++ "settlement-town-hill.png"),
    (TownMountain   , textureDirectory ++ "settlement-town-mountain.png"),
    (TownRiver      , textureDirectory ++ "settlement-town-river.png"),
    (Village        , textureDirectory ++ "settlement-village.png"),
    (VillageForest  , textureDirectory ++ "settlement-village-forest.png"),
    (VillageMountain, textureDirectory ++ "settlement-village-mountain.png"),
    -- Water
    (Water          , textureDirectory ++ "sea.png")
  ]

makeModels :: (ContextHandler ctx, MonadIO m)
  => ContextT ctx os m (Map Tile (Model os))
makeModels = do
  vertexBuffer :: Buffer os BVertex <- newBuffer . length $ tileVertices
  writeBuffer vertexBuffer 0 tileVertices

  let baseColour = V4 0.02 0.01 0.05 1

      makeModel path = do
        texture <- fromPng path
        return . Model (Material texture baseColour) TriangleFan $ vertexBuffer

  fmap M.fromList . mapM (traverse makeModel) $ textureFilePaths

  -- Create vertices (positions and UVs) of the regular hexagon with unit size
-- (outer diameter) in 2D space.
tileVertices :: [(V3 Float, V2 Float)]
tileVertices =
  -- Transform the tile vertices onto the 3d x-z plane.
  fmap (first (\(V2 x z) -> V3 x 0 z)) [c, ne, n, nw, sw, s, se, ne]
 where
  --    position         u    v
  -- Hexagon centre
  c  = (0           , V2 0.5  0.5 )
  -- The corners of the hexagon
  ne = (hexNorthEast, V2 0    0.25)
  n  = (hexNorth    , V2 0.5  0   )
  nw = (hexNorthWest, V2 1    0.25)
  se = (hexSouthEast, V2 0    0.75)
  s  = (hexSouth    , V2 0.5  1   )
  sw = (hexSouthWest, V2 1    0.75)

  hexNorthEast = V2 (w / 2) (1 / 2)
   where
    w = sqrt 3

  hexNorth = V2 0 1

  hexNorthWest = V2 (-w / 2) (1 / 2)
   where
    w = sqrt 3

  hexSouthEast = V2 (w / 2) (-1 / 2)
   where
    w = sqrt 3

  hexSouth = V2 0 (-1)

  hexSouthWest = V2 (-w / 2) (-1 / 2)
   where
    w = sqrt 3

-- Make the instance data of a pointy-top oriented hextile board with unit size
-- (half the height of a tile). where the origin is in the middle of the board
-- and the even rows are offset by one half width.
instances :: Board -> Map Tile [M44 Float]
instances board =
  M.foldlWithKey' insertInstance M.empty . boardTiles $ board
 where
  insertInstance m idx t =
    M.insertWith
      (++)
      t
      [translate . to3d . tileMapTranslation (boardSize board) $ idx]
      m

  to3d :: Num a => V2 a -> V3 a
  to3d (V2 x z) = V3 x 0 z

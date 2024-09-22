module App.Graphics.Board.Tile (
  makeModels
) where

import Control.Arrow
import Control.Monad.IO.Class
import Data.Map (Map)
import qualified Data.Map as M
import Graphics.GPipe

import App.Game.Board
import App.Graphics.Env
import App.Graphics.Texture
import App.Matrix

textureDirectory :: FilePath
textureDirectory = "assets/textures/hextiles/"

textureFilePaths :: [(Tile, FilePath)]
textureFilePaths = [
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
    (Hills1         , textureDirectory ++ "hills-1.png"),
    (Hills2         , textureDirectory ++ "hills-2.png"),
    (HillsLow1      , textureDirectory ++ "hills-low-1.png"),
    (HillsLow2      , textureDirectory ++ "hills-low-2.png"),
    (HillsLowForest , textureDirectory ++ "hills-low-forest.png"),
    (Mountain1      , textureDirectory ++ "mountain-1.png"),
    (Mountain2      , textureDirectory ++ "mountain-2.png"),
    (Tor            , textureDirectory ++ "tor.png"),
    -- Settlements
    (City           , textureDirectory ++ "settlement-city.png"),
    (CityMountain   , textureDirectory ++ "settlement-city-mountain.png"),
    (TownHill       , textureDirectory ++ "settlement-town-hill.png"),
    (TownMountain   , textureDirectory ++ "settlement-town-mountain.png"),
    (TownRiver      , textureDirectory ++ "settlement-town-river.png"),
    (Village        , textureDirectory ++ "settlement-village.png"),
    (VillageForest  , textureDirectory ++ "settlement-village-forest.png"),
    (VillageMountain, textureDirectory ++ "settlement-village-mountain.png")
  ]

makeModels :: (ContextHandler ctx, MonadIO m)
  => Board
  -> ContextT ctx os m (Map Tile (Model os, [M44 Float]))
makeModels board = do
  vertexBuffer :: Buffer os BVertex <- newBuffer . length $ tileVertices
  writeBuffer vertexBuffer 0 tileVertices

  let instances = tileInstances board

      baseColour = V4 0 0 0 1

      makeModel path = do
        texture <- fromPng path
        return . Model (Material texture baseColour) TriangleFan $ vertexBuffer

  models <- fmap M.fromList . mapM (traverse makeModel) $ textureFilePaths

  return . M.intersectionWithKey (const (,)) models $ instances

  -- Create vertices (positions and UVs) of the regular hexagon with unit size
-- (outer diameter) in 2D space.
tileVertices :: [(V3 Float, V2 Float)]
tileVertices =
  -- Transform the tile vertices onto the 3d x-z plane.
  fmap (first (\(V2 x z) -> V3 x 0 z)) [c, ne, n, nw, sw, s, se, ne]
 where
  --    position         u    v
  -- Hexagon centre
  c  = (0, V2 0.5  0.5 )
  -- The corners of the hexagon
  ne = (hexNorthEast, V2 0    0.75)
  n  = (hexNorth    , V2 0.5  1   )
  nw = (hexNorthWest, V2 1    0.75)
  se = (hexSouthEast, V2 0    0.25)
  s  = (hexSouth    , V2 0.5  0   )
  sw = (hexSouthWest, V2 1    0.25)

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
tileInstances :: Board -> Map Tile [M44 Float]
tileInstances board =
  M.foldlWithKey' insertInstance M.empty . boardTiles $ board
 where
  insertInstance m xy t = M.insertWith (++) t [translate . offset $ xy] m

  (nx, ny)      = boardSize board
  offset (x, y) = V3 (xOffset x y) 0 (yOffset y)

  xOffset x y | even y    = (x' - ((nx' - 1) / 2))       * w
              | otherwise = (x' - ((nx' - 1) / 2) + 0.5) * w
   where
    w   = sqrt 3
    x'  = fromIntegral x
    nx' = fromIntegral nx

  yOffset y = (y' - ((ny' - 1) / 2)) * h
   where
    h   = 3 / 2
    y'  = fromIntegral y
    ny' = fromIntegral ny

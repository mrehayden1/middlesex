module App.Game.Board (
  Board,
  boardSize,
  boardTiles,

  Tile(..),

  makeBoard
) where

import Control.Monad
import Control.Monad.Random
import Data.Map as M

data Tile =
      Deadland1
    | Deadland2
    | Fields1
    | Fields2
    | Forest1
    | Forest2
    | ForestSparse1
    | ForestSparse2
    | Grass1
    | Grass2
    | GrassWindmills
    | Hills1
    | Hills2
    | HillsLow1
    | HillsLow2
    | HillsLowForest
    | Mountain1
    | Mountain2
    | Tor
    | City
    | CityMountain
    | TownHill
    | TownMountain
    | TownRiver
    | Village
    | VillageForest
    | VillageMountain
  deriving (Eq, Ord, Enum, Bounded)

instance Random Tile where
  random g          =
    let lo = minBound :: Tile
        hi = maxBound :: Tile
        (t, g') = randomR (fromEnum lo, fromEnum hi) g
    in (toEnum t, g')
  randomR (lo, hi) g =
    let (t, g') = randomR (fromEnum lo, fromEnum hi) g
    in (toEnum t, g')

data Board = Board {
       boardSize'  :: (Int, Int),
       boardTiles' :: Map (Int, Int) Tile
     }

boardTiles :: Board -> Map (Int, Int) Tile
boardTiles = boardTiles'

boardSize :: Board -> (Int, Int)
boardSize = boardSize'

-- Creates a list of hex-grid tile coordinates in an offset coordinate system
-- for the given width and height.
-- See https://www.redblobgames.com/grids/hexagons/ for more.
makeBoard :: RandomGen r => Int -> Int -> Rand r Board
makeBoard w h = do
  let coords = do
        y <- [0..(h - 1)]
        x <- [0..(w - 1)]
        guard $ even y || x /= w - 1
        return (x, y)
  tiles <- mapM ((<$> getRandom) . (,)) coords
  return . Board (w, h) . M.fromList $ tiles

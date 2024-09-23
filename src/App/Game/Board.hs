module App.Game.Board (
  Board,
  boardSize,
  boardTiles,

  Tile(..),
  tileOffset,

  makeBoard
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Data.Map as M
import Numeric.Noise.Perlin

import Debug.Trace

type BoardSize = (Int, Int)

data Tile =
    -- Land
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
    | HillsHigh
    | Hills
    | HillsLow1
    | HillsLow2
    | HillsLowForest
    | Mountain
    | Tor
    | TorLow
    -- Settlements
    | City
    | CityMountain
    | TownHill
    | TownMountain
    | TownRiver
    | Village
    | VillageForest
    | VillageMountain
    -- Water
    | Water
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

-- The x-y offset of a tile in a pointy-top hextile board in outer radiuses.
tileOffset :: BoardSize -> Int -> Int -> (Float, Float)
tileOffset (nx, ny) i j = (xOffset i j, yOffset j)
 where
  xOffset x y | even y    = (x' - ((nx' - 1) / 2))       * w
              | otherwise = (x' - ((nx' - 1) / 2) + 0.5) * w
   where
    w   = sqrt 3
    x'  = fromIntegral x
    nx' = fromIntegral nx

  yOffset y = (y' - ((ny' - 1) / 2)) * (3 / 2)
   where
    y'  = fromIntegral y
    ny' = fromIntegral ny

data Board = Board {
       boardSize'  :: BoardSize,
       boardTiles' :: Map (Int, Int) Tile
     }

boardTiles :: Board -> Map (Int, Int) Tile
boardTiles = boardTiles'

boardSize :: Board -> BoardSize
boardSize = boardSize'

-- Creates a list of hex-grid tile coordinates in an offset coordinate system
-- for the given width and height and seed value.
-- See https://www.redblobgames.com/grids/hexagons/ for more.
makeBoard :: Int -> Int -> Int -> Board
makeBoard w h seed =
  let seed' = seed + (w * h) -- Make a unique seed for maps of different sizes
  in evalRand (genBoard w h) . mkStdGen $ seed'

genBoard :: RandomGen r => Int -> Int -> Rand r Board
genBoard w h = do
  let coords = do
        y <- [0..(h - 1)]
        x <- [0..(w - 1)]
        guard $ even y || x /= w - 1
        return (x, y)
      -- Tile elevation
  elevationSeed <- getRandom
  let elevationDist = perlin elevationSeed 1 0.5 0.5
      elevation = normalise . flip fmap coords $
                  \(x, y) -> (/ 2) . (+ 1) . noiseValue elevationDist
                     $ (fromIntegral x, fromIntegral y, 0)
      -- Tree coverage
  foliageSeed <- getRandom
  let foliageDist = perlin foliageSeed 1 0.2 0.5
      foliage = normalise . flip fmap coords $
                  \(x, y) -> (/ 2) . (+ 1) . noiseValue foliageDist
                     $ (fromIntegral x, fromIntegral y, 0)

  tiles <- fmap getZipList . sequence $
             (\xy e f -> fmap (xy, ) . chooseTile e $ f)
               <$> ZipList coords
               <*> ZipList elevation
               <*> ZipList foliage

  return . Board (w, h) . M.fromList $ tiles
 where
   chooseTile elev foliage
     | elev > 0.9                         = return Mountain
     | elev > 0.8                         = return HillsHigh
     | elev > 0.7                         = return Hills
     | elev > 0.6
         && foliage > foliageMedThreshold = return HillsLowForest
     | elev > 0.6
         && foliage > foliageHiThreshold  = getRandomR (Forest1, Forest2)
     | elev > 0.6
         && foliage > foliageMedThreshold = getRandomR (ForestSparse1, ForestSparse2)
     | elev > 0.6                         = getRandomR (HillsLow1, HillsLow2)
     | elev > 0.1                         = getRandomR (Grass1, Grass2)
     | otherwise                          = return Water

   foliageHiThreshold = 0.7
   foliageMedThreshold = 0.6

normalise :: (Ord a, Fractional a) => [a] -> [a]
normalise xs =
  let range = maximum xs - minimum xs
  in fmap ((/ range) . subtract (minimum xs)) xs

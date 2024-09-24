module App.Game.Board (
  Board,
  boardSize,
  boardTiles,

  Tile(..),
  tileMapTranslation,

  genBoard,

  toAxial,
  roundAxial,

  fromAxial
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Data.Bool
import Data.Map as M
import Linear
import Numeric.Noise.Perlin

import App.Math

-- A pointy-top hex tile board using an "offset" co-ordinate system, where each
-- odd row is (boardWidth - 1) long and offset by a half tile width in
-- Cartesian space.
--
-- See https://www.redblobgames.com/grids/hexagons/ for more.
data Board = Board {
       boardSize'  :: BoardSize,
       boardTiles' :: Map (Int, Int) Tile
     }

type BoardSize = (Int, Int)

boardTiles :: Board -> Map (Int, Int) Tile
boardTiles = boardTiles'

boardSize :: Board -> BoardSize
boardSize = boardSize'

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

-- Useful for picking random tiles that are adjacent in the derived Enum
-- instance (order of definition).
instance Random Tile where
  random g          =
    let lo = minBound :: Tile
        hi = maxBound :: Tile
        (t, g') = randomR (fromEnum lo, fromEnum hi) g
    in (toEnum t, g')
  randomR (lo, hi) g =
    let (t, g') = randomR (fromEnum lo, fromEnum hi) g
    in (toEnum t, g')


-- Creates a `Board` for the given height, width and seed value.
genBoard :: Int -> Int -> Int -> Board
genBoard w h seed =
  let seed' = seed + (w * h) -- Make a unique seed for maps of different sizes
  in evalRand (genBoard' w h) . mkStdGen $ seed'

genBoard' :: RandomGen r => Int -> Int -> Rand r Board
genBoard' w h = do
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
     | elev > 0.6                         = getRandomR (HillsLow1, HillsLow2)
     | elev > 0.1
         && foliage > foliageHiThreshold  = getRandomR (Forest1, Forest2)
     | elev > 0.1
         && foliage > foliageMedThreshold = getRandomR (ForestSparse1, ForestSparse2)
     | elev > 0.1                         = getRandomR (Grass1, Grass2)
     | otherwise                          = return Water

   foliageHiThreshold = 0.7
   foliageMedThreshold = 0.6

-- The cartesian offset of a tile index from a pointy-top hextile board in tile
-- outer-radiuses in map space.
tileMapTranslation :: BoardSize -> (Int, Int) -> V2 Float
tileMapTranslation (nx, ny) (i, j) = V2 (xOffset i j) (yOffset j)
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

-- Convert standard tile offset-indices to axial co-ordinate tile indices.
-- See https://www.redblobgames.com/grids/hexagons/#coordinates-axial for more.
toAxial :: Floating a => (a, a) -> (a, a)
toAxial (i, j) =
  let q = i * (sqrt 3 / 3) - j / 3
      r = j * 2 / 3
  in (q, r)

-- Round floating point hex grid axial (q + r) coordinates.
roundAxial :: (Floating a, RealFrac a) => (a, a) -> (Int, Int)
roundAxial (q, r) =
  let q' = round q
      r' = round r
      qRem = q - fromIntegral q'
      rRem = r - fromIntegral r'
      dq = round (qRem + 0.5 * rRem) * bool 0 1 (qRem ** 2 >= rRem ** 2)
      dr = round (rRem + 0.5 * qRem) * bool 0 1 (qRem ** 2 <  rRem ** 2)
  in (q' + dq, r' + dr)

fromAxial :: (Integral a) => (a, a) -> (a, a)
fromAxial (q, r) =
  let i = q + ((r - (bool 1 0 . even $ r)) `div` 2)
      j = r
  in (i, j)

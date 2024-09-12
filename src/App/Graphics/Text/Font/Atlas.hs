module App.Graphics.Text.Font.Atlas (
  Atlas(..),
  decodeAtlasFromFile,

  Variant(..),
  AtlasMeta(..),
  YOrigin(..),
  Metrics(..),
  GlyphMap(..),
  Glyph(..),
  Bounds(..),

  TextureWidth,
  TextureHeight
) where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.IntMap.Strict as IM
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics


type TextureWidth = Int;
type TextureHeight = Int;



decodeAtlasFromFile :: FilePath -> IO (Either String Atlas)
decodeAtlasFromFile = eitherDecodeFileStrict

data Atlas = Atlas {
  atlasMeta :: AtlasMeta,
  variants :: Vector Variant
} deriving (Show, Generic)

instance FromJSON Atlas where
  parseJSON v = multipleVariants v <|> singleVariant v
   where
    multipleVariants = withObject "Atlas" $ \v' -> Atlas
      <$> v' .: "atlas"
      <*> v' .: "variants"

    singleVariant = withObject "Atlas" $ \v' -> do
      s <- Variant <$> v' .: "metrics" <*> v' .: "glyphs"
      Atlas <$> v' .: "atlas" <*> pure (V.singleton s)

data Variant = Variant {
  metrics :: Metrics,
  glyphs :: GlyphMap
} deriving (Show, Generic)

instance FromJSON Variant

data AtlasMeta = AtlasMeta {
  distanceRange :: Float,
  distanceRangeMiddle :: Float,
  size :: Float,
  width :: TextureWidth,
  height :: TextureHeight,
  yOrigin :: YOrigin
} deriving (Show, Generic)

instance FromJSON AtlasMeta where
  parseJSON = withObject "AtlasMeta" $ \v -> AtlasMeta
    <$> v .: "distanceRange"
    <*> v .: "distanceRangeMiddle"
    <*> v .: "size"
    <*> v .: "width"
    <*> v .: "height"
    <*> v .: "yOrigin"

data YOrigin = YBottom | YTop
  deriving (Eq, Show, Generic)

instance FromJSON YOrigin where
  parseJSON = withText "YOrigin" parseYOrigin
   where
    parseYOrigin t | t == "bottom" = return YBottom
                   | t == "top"    = return YTop
                   | otherwise     = parseFail "Unrecognised yOrigin"

data Metrics = Metrics {
  emSize :: Int,
  lineHeight :: Float,
  ascender :: Float,
  descender :: Float,
  underlineY :: Float,
  underlineThickness :: Float
} deriving (Show, Generic)

instance FromJSON Metrics

newtype GlyphMap = GlyphMap { unGlyphMap :: IntMap Glyph }
  deriving (Show)

instance FromJSON GlyphMap where
  parseJSON = withArray "GlyphMap" $ \a -> do
    gs <- fmap V.toList . mapM parseJSON $ a
    return . GlyphMap . IM.fromList . fmap ((,) <$> unicode <*> id) $ gs

data Glyph = Glyph {
  unicode :: Int,
  advance :: Float,
  planeBounds :: Maybe Bounds,
  atlasBounds :: Maybe Bounds
} deriving (Show)

instance FromJSON Glyph where
  parseJSON = withObject "Glyph" $ \v -> Glyph
    <$> v .:  "unicode"
    <*> v .:  "advance"
    <*> v .:? "planeBounds"
    <*> v .:? "atlasBounds"

data Bounds = Bounds {
  top :: Float,
  right :: Float,
  bottom :: Float,
  left :: Float
} deriving (Show, Generic)

instance FromJSON Bounds

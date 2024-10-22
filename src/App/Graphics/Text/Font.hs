module App.Graphics.Text.Font (
  Font(..),
  loadFont,

  Advance,
  GlyphVertex,

  TypefaceIx,
  typefaceIxLabel,
  typefaceIxBody,
  typefaceIxHeading,
  typefaceIxDebug,

  Typeface(..),
  Metrics(..)
) where


import Control.Monad.IO.Class
import Data.IntMap (IntMap)
import Data.Maybe
import Data.Vector (Vector)
import Graphics.GPipe

import App.Graphics.Text.Font.Atlas
import App.Graphics.Texture

metadataFilePath :: FilePath
metadataFilePath = "assets/fonts/atlas.json"

glyphsFilePath :: FilePath
glyphsFilePath = "assets/fonts/glyphs.png"

type TypefaceIx = Int

typefaceIxLabel, typefaceIxBody, typefaceIxHeading, typefaceIxDebug :: TypefaceIx

typefaceIxLabel   = 0
typefaceIxBody    = 1
typefaceIxHeading = 1
typefaceIxDebug   = 2

-- An MSDF font, which has one or more typefaces.
--
-- The vertex buffers (geometry & texture co-ordinates) and "advance" of each
-- glyph are stored along with the glyph texture.
--
-- When loaded a font's textures and vertex buffers are immediately stored on
-- graphics memory.
data Font os = Font {
  fontGlyphTexture :: Texture2D os (Format RGBAFloat),
  fontSize :: Float,
  fontSdfUnitRange :: Float,
  -- Glyph geometry and the advance (the distance to move the cursor to draw
  -- the next character).
  fontTypefaces :: Vector (Typeface os)
}

data Typeface os = Typeface {
    typefaceGlyphs :: Glyphs os,
    typefaceMetrics :: Metrics
  }

type Advance = Float

type Glyphs os = IntMap ([GlyphVertex], Advance)

-- Vertices (positions & texture co-ordinates
type GlyphVertex = (V2 Float, V2 Float)


loadFont :: (ContextHandler ctx, MonadIO m)
  => ContextT ctx os m (Font os)
loadFont = do
  Atlas{..} <- liftIO . fmap (either error id)
    . decodeAtlasFromFile $ metadataFilePath
  -- Load the glyphs into the glyph texture
  texture <- fromPng' RGB 1 glyphsFilePath
  let faces = fmap (makeTypeface atlasMeta) variants
      AtlasMeta{..} = atlasMeta
  return $ Font texture size distanceRange faces

makeTypeface :: AtlasMeta -> Variant -> Typeface os
makeTypeface meta Variant{..} =
  let gs = fmap (makeGlyph meta) . unGlyphMap $ glyphs
  in Typeface gs metrics

-- Creates glpyh geometry relative to a cursor at the origin and returns the
-- advance of the cursor for the next character.
makeGlyph :: AtlasMeta -> Glyph -> ([GlyphVertex], Advance)
makeGlyph AtlasMeta{..} Glyph{..} =
  let quad = fromMaybe [] $ makeQuad <$> planeBounds <*> atlasBounds
  in (quad, advance)
 where
  makeQuad :: Bounds -> Bounds -> [GlyphVertex]
  makeQuad pBounds aBounds =
    --  x, y - the glyph's quad geometry in ems
    let x0 = left pBounds
        x1 = right pBounds
        y0 = top pBounds
        y1 = bottom pBounds

        w = fromIntegral width
        h = fromIntegral height

    --  u, v - the glyph's atlas texture co-ordinates in OpenGL uv space.
        u0 = left aBounds / w
        u1 = right aBounds / w

        V2 v0 v1 = case yOrigin of
          YBottom -> V2 (    top aBounds) (    bottom aBounds) ^/ h
          YTop    -> V2 (h - top aBounds) (h - bottom aBounds) ^/ h

    in [ (V2 x0 y0, V2 u0 v0),
         (V2 x1 y1, V2 u1 v1),
         (V2 x1 y0, V2 u1 v0),
         (V2 x0 y0, V2 u0 v0),
         (V2 x0 y1, V2 u0 v1),
         (V2 x1 y1, V2 u1 v1)
       ]

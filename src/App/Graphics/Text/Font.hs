module App.Graphics.Text.Font (
  Font(..),
  loadFont,

  Advance,
  GlyphVertex,

  TypefaceI,
  headingTypeface,
  debugTypeface
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

type TypefaceI = Int

headingTypeface, debugTypeface :: TypefaceI
headingTypeface = 0
debugTypeface   = 1

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
  fontTypefaces :: Vector (Glyphs os)
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
  texture <- fromPng glyphsFilePath
  let vs = fmap (makeGlyphs atlasMeta) variants
      AtlasMeta{..} = atlasMeta
  return $ Font texture size distanceRange vs

makeGlyphs :: AtlasMeta -> Variant -> Glyphs os
makeGlyphs meta Variant{..} =
  fmap (makeGlyph meta) . unGlyphMap $ glyphs

-- Creates glpyh geometry relative to a cursor at the origin and returns the
-- advance of the cursor for the next character.
makeGlyph :: AtlasMeta -> Glyph -> ([GlyphVertex], Advance)
makeGlyph AtlasMeta{..} Glyph{..} =
  let quad = fromMaybe [] $ makeQuad <$> planeBounds <*> atlasBounds
  in (quad, advance)
 where
  makeQuad :: Bounds -> Bounds -> [GlyphVertex]
  makeQuad pBounds aBounds =
    -- x, y - the glyph's quad geometry in ems
    let x0 = left pBounds
        y0 = bottom pBounds
        x1 = right pBounds
        y1 = top pBounds

        w = fromIntegral width
        h = fromIntegral height

    -- u, v - the glyph's atlas texture co-ordinates in OpenGL uv space.
        u0 = left aBounds / w
        u1 = right aBounds / w

        V2 v0 v1 = case yOrigin of
          YTop    -> V2 (    bottom aBounds) (    top aBounds) ^/ h
          YBottom -> V2 (h - bottom aBounds) (h - top aBounds) ^/ h

    in [ (V2 x0 y0, V2 u0 v0),
         (V2 x1 y0, V2 u1 v0),
         (V2 x1 y1, V2 u1 v1),
         (V2 x1 y1, V2 u1 v1),
         (V2 x0 y1, V2 u0 v1),
         (V2 x0 y0, V2 u0 v0)
       ]

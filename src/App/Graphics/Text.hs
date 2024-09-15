module App.Graphics.Text (
  Font,

  Renderer,
  initialise
) where

import Control.Arrow
import Control.Monad.Exception
import Control.Monad.IO.Class
import Data.Char
import qualified Data.IntMap as IM
import Data.Maybe
import Data.Vector ((!))
import Linear
import Graphics.GPipe hiding (Shader)

import App.Graphics.Env
import App.Graphics.Text.Font as F

type Origin = V2 Float

type TextColor = V4 Float

type Shader os = CompiledShader os (PrimitiveArray Triangles (B2 Float, B2 Float))

type Renderer ctx os m = TypefaceI -> TextColor -> Origin -> String -> ContextT ctx os m ()

initialise :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Window' os
  -> ContextT ctx os m (Renderer ctx os m)
initialise window = do
  font <- loadFont
  shader <- createShader window font
  return . renderText font $ shader

createShader :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Window' os
  -> Font os
  -> ContextT ctx os m (CompiledShader os (PrimitiveArray Triangles (B2 Float, B2 Float)))
createShader window Font{..} = do
  V2 vw vh <- getFrameBufferSize window

  compileShader $ do
    let sampleFilter = SamplerFilter Linear Linear Nearest Nothing
    sampler <- newSampler2D (const (fontGlyphTexture, sampleFilter, (pure Repeat, undefined)))

    let median r g b = maxB (minB r g) $ minB (maxB r g) b

    let calcShading uv =
          let V3 r g b = sample2D sampler SampleAuto Nothing Nothing uv
              signedDist = median r g b
              screenPxDist = realToFrac (scale * fontSdfUnitRange) * (signedDist - 0.5)
              opacity = clamp (screenPxDist + 0.5) 0 1
          in mix 0 1 (pure opacity) :: V3 (S F Float)

    let proj = ortho 0 1920 0 1080 0 1

    primitiveStream <- toPrimitiveStream id
    let primitiveStream2 = flip fmap primitiveStream $ \(V2 x y, tx) ->
          (proj !* V4 x y 0 1, tx)

    fragmentStream <- fmap (fmap calcShading)
      . flip rasterize primitiveStream2
      . const $
          (Front,
           ViewPort (V2 0 0) (V2 vw vh),
           DepthRange 0 1
          )

    drawWindowColor
      (const (window, ContextColorOption NoBlending (V3 True True True)))
      fragmentStream

renderText :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Font os
  -> Shader os
  -> Renderer ctx os m
renderText Font{..} shader iVar clr orig str = do
  let glyphs  = fontTypefaces ! iVar
      def     = fromMaybe (defaultGlyphErr iVar) . IM.lookup (ord '?') $ glyphs
      glyphs' = fst . foldl accumGlyph ([], 0) $ str
      transform c = first $ (+ orig) . (^* (fontSize * scale)) . (+ V2 c 0)

      accumGlyph (qs, cursor) char =
        let (glyph, advance) = fromMaybe def . IM.lookup (ord char) $ glyphs
            glyph' = fmap (transform cursor) glyph
        in (qs ++ glyph', cursor + advance)

  vertexBuffer <- newBuffer . length $ glyphs'
  writeBuffer vertexBuffer 0 glyphs'
  render $ do
    vertexArray <- newVertexArray vertexBuffer
    let primitiveArray = toPrimitiveArray TriangleList vertexArray
    shader primitiveArray

 where

  defaultGlyphErr _ = error . (msg ++) . show $ iVar
   where
    msg = "createText: Default glyph '?' not found in atlas for font variant "

scale :: Float
scale = 5

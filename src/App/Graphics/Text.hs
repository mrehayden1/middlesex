module App.Graphics.Text (
  Font,

  TextRenderer,
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
import Graphics.GPipe

import App.Graphics.Text.Font as F
import App.Graphics.Window


type Shader' os = CompiledShader os (ShaderEnv os)

data ShaderEnv os = ShaderEnv {
    shaderTexture :: Texture2D os (Format RGBAFloat),
    shaderPrimitives :: PrimitiveArray Triangles BVertex,
    shaderUniforms :: Buffer os (Uniform UniformsB)
  }

type BVertex = (B2 Float, B2 Float)


data Uniforms = Uniforms {
    baseColour :: V4 Float,
    scale :: Float
  }

data UniformsB = UniformsB {
    baseColourB :: B4 Float,
    scaleB :: B Float
  }

data UniformsS x = UniformsS {
    baseColourS :: V4 (S x Float),
    scaleS :: S x Float
  }

instance BufferFormat UniformsB where
  type HostFormat UniformsB = Uniforms
  toBuffer = proc ~(Uniforms{..}) -> do
               baseColour' <- toBuffer -< baseColour
               scale' <- toBuffer -< scale
               returnA -< UniformsB {
                              baseColourB = baseColour',
                              scaleB = scale'
                            }

instance UniformInput UniformsB where
  type UniformFormat UniformsB x = UniformsS x
  toUniform = proc ~(UniformsB{..}) -> do
                baseColour' <- toUniform -< baseColourB
                scale' <- toUniform -< scaleB
                returnA -< UniformsS {
                               baseColourS = baseColour',
                               scaleS = scale'
                             }

type TextRenderer ctx os m = TypefaceI -> TextColor -> PixelSize -> Origin
       -> String -> ContextT ctx os m ()

type Origin = V2 Float

type TextColor = V4 Float

type PixelSize = Float

maxGlyphVertices :: Int
maxGlyphVertices = 6 * 1024


initialise :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Window' os
  -> WindowSize
  -> ContextT ctx os m (TextRenderer ctx os m)
initialise window windowSize = do
  font <- loadFont
  shader <- createShader window windowSize font

  vertexBuffer :: Buffer os (B2 Float, B2 Float) <- newBuffer maxGlyphVertices
  uniformsBuffer :: Buffer os (Uniform UniformsB) <- newBuffer 1

  return . createTextRender vertexBuffer uniformsBuffer font $ shader

createShader :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Window' os
  -> WindowSize
  -> Font os
  -> ContextT ctx os m (Shader' os)
createShader window (vw, vh) Font{..} =
  compileShader $ do
    let sampleFilter = SamplerFilter Linear Linear Nearest Nothing
    sampler <- newSampler2D $ const
                 (fontGlyphTexture, sampleFilter, (pure Repeat, undefined))

    let median r g b = maxB (minB r g) $ minB (maxB r g) b

    let proj = ortho 0 (realToFrac vw) 0 (realToFrac vh) 0 1

    -- Uniforms
    fragmentUniforms <- getUniform ((, 0) . shaderUniforms)

    let baseColour = baseColourS fragmentUniforms
        scale      = scaleS fragmentUniforms

    primitiveStream <- toPrimitiveStream shaderPrimitives
    let primitiveStream' = flip fmap primitiveStream $ \(V2 x y, tx) ->
          (proj !* V4 x y 0 1, tx)

    let shadeFragment uv =
          let V4 r g b _ = sample2D sampler SampleAuto Nothing Nothing uv
              signedDist = median r g b
              screenPxDist = (scale * realToFrac fontSdfUnitRange)
                               * (signedDist - 0.5)
              opacity = clamp (screenPxDist + 0.5) 0 1
          in mix 0 baseColour (pure opacity) :: V4 (S F Float)

    fragmentStream <- fmap (fmap shadeFragment)
      . flip rasterize primitiveStream'
      . const $
          (Front,
           ViewPort (V2 0 0) (V2 vw vh),
           DepthRange 0 1
          )

    drawWindowColor
      (const (window, ContextColorOption blending (pure True)))
      fragmentStream
 where
  blending =
    BlendRgbAlpha
      (FuncAdd, FuncAdd)
      (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors One Zero)
      0

createTextRender :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Buffer os (B2 Float, B2 Float)
  -> Buffer os (Uniform UniformsB)
  -> Font os
  -> Shader' os
  -> TextRenderer ctx os m
createTextRender vertexBuffer uniformsBuffer Font{..} shader iVar clr pixelSize orig str = do
  let glyphs  = fontTypefaces ! iVar
      def     = fromMaybe (defaultGlyphErr iVar) . IM.lookup (ord '?') $ glyphs
      glyphs' = fst . foldl accumGlyph ([], 0) $ str
      transform c = first $ (+ orig) . (^* pixelSize) . (+ V2 c 0)
      scale = pixelSize / fontSize

      accumGlyph (qs, cursor) char =
        let (glyph, advance) = fromMaybe def . IM.lookup (ord char) $ glyphs
            glyph' = fmap (transform cursor) glyph
        in (qs ++ glyph', cursor + advance)

  writeBuffer vertexBuffer 0 glyphs'

  let uniforms = Uniforms {
          baseColour = clr,
          scale = scale
        }

  writeBuffer uniformsBuffer 0 [uniforms]

  render $ do
    vertexArray <- fmap (takeVertices (length glyphs')) . newVertexArray
                     $ vertexBuffer
    let primitiveArray = toPrimitiveArray TriangleList vertexArray
        env = ShaderEnv {
                  shaderTexture = fontGlyphTexture,
                  shaderPrimitives = primitiveArray,
                  shaderUniforms = uniformsBuffer
                }
    shader env

 where
  defaultGlyphErr _ = error . (msg ++) . show $ iVar
   where
    msg = "createText: Default glyph '?' not found in atlas for font variant "

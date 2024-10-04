module App.Graphics.Text (
  Font,
  loadFont,
  typefaceMain,
  typefaceDebug,

  Text,
  createText,

  TextRenderer,
  initialise
) where

import Control.Arrow
import Control.Lens
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

type Vertex  = (V2 Float, V2 Float)
type BVertex = (B2 Float, B2 Float)


data Uniforms = Uniforms {
    baseColour :: V4 Float,
    -- Distance field range in screen pixels
    screenPixelRange :: Float
  }

data UniformsB = UniformsB {
    baseColourB :: B4 Float,
    screenPixelRangeB :: B Float
  }

data UniformsS x = UniformsS {
    baseColourS :: V4 (S x Float),
    screenPixelRangeS :: S x Float
  }

instance BufferFormat UniformsB where
  type HostFormat UniformsB = Uniforms
  toBuffer = proc ~(Uniforms{..}) -> do
               baseColour' <- toBuffer -< baseColour
               screenPixelRange' <- toBuffer -< screenPixelRange
               returnA -< UniformsB {
                              baseColourB = baseColour',
                              screenPixelRangeB = screenPixelRange'
                            }

instance UniformInput UniformsB where
  type UniformFormat UniformsB x = UniformsS x
  toUniform = proc ~(UniformsB{..}) -> do
                baseColour' <- toUniform -< baseColourB
                screenPixelRange' <- toUniform -< screenPixelRangeB
                returnA -< UniformsS {
                               baseColourS = baseColour',
                               screenPixelRangeS = screenPixelRange'
                             }

type TextRenderer ctx os m = TextColour -> Text os -> ContextT ctx os m ()

type Origin = V2 Float

type TextColour = V4 Float

type PixelSize = Float

maxGlyphVertices :: Int
maxGlyphVertices = 6 * 1024


initialise :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Window' os
  -> WindowSize
  -> ContextT ctx os m (TextRenderer ctx os m)
initialise window windowSize = do
  shader <- createShader window windowSize

  vertexBuffer :: Buffer os (B2 Float, B2 Float) <- newBuffer maxGlyphVertices
  uniformsBuffer :: Buffer os (Uniform UniformsB) <- newBuffer 1

  return . createTextRender vertexBuffer uniformsBuffer $ shader

createShader :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Window' os
  -> WindowSize
  -> ContextT ctx os m (Shader' os)
createShader window (vw, vh) =
  compileShader $ do
    let sampleFilter = SamplerFilter Linear Linear Nearest Nothing
    sampler <- newSampler2D $ \s ->
                 (shaderTexture s, sampleFilter, (pure Repeat, undefined))

    let median r g b = maxB (minB r g) $ minB (maxB r g) b

    let proj = ortho 0 (realToFrac vw) 0 (realToFrac vh) 0 1

    -- Uniforms
    fragmentUniforms <- getUniform ((, 0) . shaderUniforms)

    let baseColour = baseColourS fragmentUniforms
        screenPixelRange = screenPixelRangeS fragmentUniforms

    primitiveStream <- toPrimitiveStream shaderPrimitives
    let primitiveStream' = flip fmap primitiveStream $ \(V2 x y, tx) ->
          (proj !* V4 x y 0 1, tx)

    let shadeFragment uv =
          let V4 r g b _ = sample2D sampler SampleAuto Nothing Nothing uv
              signedDist = median r g b
              screenPxDist = screenPixelRange * (signedDist - 0.5)
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

data Text os = Text {
    textFont     :: Font os,
    textSize     :: PixelSize,
    textVertices :: [Vertex]
  }

createText :: Font os -> TypefaceI -> PixelSize -> Origin -> String -> Text os
createText font@Font{..} face pixelSize orig str =
  let glyphMap = fontTypefaces ! face
      def      = fromMaybe (defaultGlyphErr face) . IM.lookup (ord '?')
                   $ glyphMap

      transform c = first $ (+ orig) . (^* pixelSize) . (+ V2 c 0)

      accumGlyph (qs, cursor) char =
        let (glyph, advance) = fromMaybe def . IM.lookup (ord char) $ glyphMap
            glyph' = fmap (transform cursor) glyph
        in (qs ++ glyph', cursor + advance)

   in Text {
          textFont     = font,
          textSize     = pixelSize,
          textVertices = fst . foldl accumGlyph ([], 0) $ str
        }
 where
  defaultGlyphErr _ = error . (msg ++) . show $ face
   where
    msg = "createText: Default glyph '?' not found in atlas for typeface "

createTextRender :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Buffer os (B2 Float, B2 Float)
  -> Buffer os (Uniform UniformsB)
  -> Shader' os
  -> TextColour
  -> Text os
  -> ContextT ctx os m ()
createTextRender vertexBuffer uniformsBuffer shader clr Text{..} = do
  writeBuffer vertexBuffer 0 textVertices

  let uniforms = Uniforms {
          baseColour = clr,
          screenPixelRange = (textSize / fontSize textFont)
                               * fontSdfUnitRange textFont
        }

  writeBuffer uniformsBuffer 0 [uniforms]

  render $ do
    vertexArray <- fmap (takeVertices (length textVertices)) . newVertexArray
                     $ vertexBuffer
    let primitiveArray = toPrimitiveArray TriangleList vertexArray
        env = ShaderEnv {
                  shaderTexture = fontGlyphTexture textFont,
                  shaderPrimitives = primitiveArray,
                  shaderUniforms = uniformsBuffer
                }
    shader env

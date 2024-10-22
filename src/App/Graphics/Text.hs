module App.Graphics.Text (
  Font,
  loadFont,

  typefaceIxLabel,
  typefaceIxBody,
  typefaceIxHeading,
  typefaceIxDebug,

  SDFText(sdfTextWidth),
  sdfTextHeight,

  uiSdfText,

  Renderer,
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
import Linear.Affine
import Graphics.GPipe
import Text.Printf

import App.Graphics.Text.Font as F
import App.Graphics.Window
import App.Math
import qualified App.Math.Matrix as Matrix


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
    scale :: Float,
    -- Distance field range in screen pixels
    screenPixelRange :: Float,
    textOrigin :: Origin
  }

data UniformsB = UniformsB {
    baseColourB :: B4 Float,
    scaleB :: B Float,
    screenPixelRangeB :: B Float,
    textOriginB :: B2 Float
  }

data UniformsS x = UniformsS {
    baseColourS :: V4 (S x Float),
    scaleS :: S x Float,
    screenPixelRangeS :: S x Float,
    textOriginS :: V2 (S x Float)
  }

instance BufferFormat UniformsB where
  type HostFormat UniformsB = Uniforms
  toBuffer = proc ~(Uniforms{..}) -> do
               baseColour' <- toBuffer -< baseColour
               scale' <- toBuffer -< scale
               screenPixelRange' <- toBuffer -< screenPixelRange
               textOrigin' <- toBuffer -< unP textOrigin
               returnA -< UniformsB {
                              baseColourB = baseColour',
                              scaleB = scale',
                              screenPixelRangeB = screenPixelRange',
                              textOriginB = textOrigin'
                            }

instance UniformInput UniformsB where
  type UniformFormat UniformsB x = UniformsS x
  toUniform = proc ~(UniformsB{..}) -> do
                baseColour' <- toUniform -< baseColourB
                scale' <- toUniform -< scaleB
                screenPixelRange' <- toUniform -< screenPixelRangeB
                textOrigin' <- toUniform -< textOriginB
                returnA -< UniformsS {
                               baseColourS = baseColour',
                               scaleS = scale',
                               screenPixelRangeS = screenPixelRange',
                               textOriginS = textOrigin'
                             }

type Renderer ctx os m = SDFText os -> Origin -> Float -> ContextT ctx os m ()

type Origin = Point V2 Float

type Colour = V4 Float

type PixelSize = Float

maxGlyphVertices :: Int
maxGlyphVertices = 6 * 1024


initialise :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Window' os
  -> WindowSize
  -> ContextT ctx os m (Renderer ctx os m)
initialise window windowSize = do
  shader <- createShader window windowSize

  vertexBuffer :: Buffer os (B2 Float, B2 Float) <- newBuffer maxGlyphVertices
  uniformsBuffer :: Buffer os (Uniform UniformsB) <- newBuffer 1

  return . createRenderer vertexBuffer uniformsBuffer $ shader

createShader :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Window' os
  -> WindowSize
  -> ContextT ctx os m (Shader' os)
createShader window (vw, vh) =
  compileShader $ do
    let sampleFilter = SamplerFilter Linear Linear Linear Nothing
    sampler <- newSampler2D $ \s ->
                 (shaderTexture s, sampleFilter, (pure Repeat, undefined))

    let median r g b = maxB (minB r g) $ minB (maxB r g) b

    -- Uniforms
    vertexUniforms <- getUniform ((, 0) . shaderUniforms)

    let V2 ox oy = textOriginS vertexUniforms
        scale = scaleS vertexUniforms


    fragmentUniforms <- getUniform ((, 0) . shaderUniforms)

    let baseColour = baseColourS fragmentUniforms
        screenPixelRange = screenPixelRangeS fragmentUniforms

    -- Vertex shader
    primitiveStream <- toPrimitiveStream shaderPrimitives
    let primitiveStream' = flip fmap primitiveStream $ \(V2 x y, uv) ->
          let vh'  = fromIntegral vh
              vw'  = fromIntegral vw
              proj = ortho 0 vw' 0 vh' 0 1
              modelM = Matrix.translate (V3 ox oy 0)
                         !*! Matrix.scale (V3 scale scale 0)
          in (proj !*! modelM !* V4 x y 0 1, uv)

    -- Fragment shader
    let shadeFragment uv =
          let V4 r g b _ = sample2D sampler SampleAuto Nothing Nothing uv
              signedDist = median r g b
              screenPxDist = screenPixelRange * (signedDist - 0.5)
              opacity = clamp (screenPxDist + 0.5) 0 1
          -- Because we're not using premultiplied alpha blending, we cheat and
          -- use the foreground colour with the alpha set to zero as the
          -- background colour we blend our glyph edges into.
          in mix (baseColour & _w .~ 0) baseColour (pure opacity)

    fragmentStream <- fmap (fmap shadeFragment)
      . flip rasterize primitiveStream'
      . const $
          (FrontAndBack,
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

data SDFText os = SDFText {
    sdfTextColour   :: Colour,
    sdfTextFaceIx   :: TypefaceIx,
    sdfTextFont     :: Font os,
    -- Line height in pixels
    sdfTextSize     :: PixelSize,
    sdfTextVertices :: [Vertex],
    -- Pixel width of all rendererd glyphs - cached
    sdfTextWidth    :: Float
  }

sdfTextHeight :: SDFText os -> Float
sdfTextHeight SDFText{..} =
  let face = fontTypefaces sdfTextFont ! sdfTextFaceIx
  in sdfTextSize * (lineHeight . typefaceMetrics $ face)

-- Text that is meant to be rendered in a UI context.
-- i.e. Geometry rounded to the nearest pixel, no anisotrophy.
uiSdfText :: Font os
  -> TypefaceIx
  -> PixelSize
  -> Colour
  -> String
  -> SDFText os
uiSdfText font@Font{..} faceIx pixelSize colour str =
  let -- We transform the glyph geometry to y-down with the origin at the top
      -- left of the line box.
      -- Since this is UI text we round verices to the nearest pixel
      accumGlyph (qs, cursor') char =
        let (glyph, advance) = fromMaybe def . IM.lookup (ord char) $ glyphs
            glyph' = fmap (transform' cursor') glyph
        in (qs ++ glyph', cursor' + advance)

      (vertices, cursor) = foldl accumGlyph ([], 0) str

   in SDFText {
          sdfTextColour   = colour,
          sdfTextFaceIx   = faceIx,
          sdfTextFont     = font,
          sdfTextSize     = pixelSize,
          sdfTextVertices = vertices,
          sdfTextWidth    = cursor * pixelSize
        }
 where
  face   = fontTypefaces ! faceIx
  glyphs = typefaceGlyphs face
  def    = fromMaybe defaultGlyphErr . IM.lookup (ord '?') $ glyphs

  transform' cur = first $ fmap roundFloat . (^* pixelSize)
                     . (+ V2 cur (-descender'))

  metrics     = typefaceMetrics face
  descender'  = descender metrics

  defaultGlyphErr = error . printf msg $ faceIx
   where
    msg = "uiSdfText: Default glyph '?' not found in atlas for typeface %d"

createRenderer :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Buffer os (B2 Float, B2 Float)
  -> Buffer os (Uniform UniformsB)
  -> Shader' os
  -> Renderer ctx os m
createRenderer vertexBuffer uniformsBuffer shader SDFText{..} origin' scale' =
  do
    writeBuffer vertexBuffer 0 sdfTextVertices

    let uniforms = Uniforms {
            baseColour = sdfTextColour,
            screenPixelRange = ((sdfTextSize * scale') / fontSize sdfTextFont)
                                 * fontSdfUnitRange sdfTextFont,
            scale = scale',
            textOrigin = origin'
          }

    writeBuffer uniformsBuffer 0 [uniforms]

    render $ do
      vertexArray <- fmap (takeVertices (length sdfTextVertices))
                       . newVertexArray $ vertexBuffer
      let primitiveArray = toPrimitiveArray TriangleList vertexArray
          env = ShaderEnv {
                    shaderTexture = fontGlyphTexture sdfTextFont,
                    shaderPrimitives = primitiveArray,
                    shaderUniforms = uniformsBuffer
                  }
      shader env

module App.Graphics.UI (
  UI(..),
  UIElem(..),

  createRenderer
) where

import Control.Monad.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Vector.Fixed as V
import Graphics.GPipe
import qualified Graphics.UI.GLFW as GLFW
import Linear.Affine

import App.Graphics.Text as Text
import App.Graphics.Texture
import App.Graphics.Window
import App.Math

-- Important Notes on UI Rendering
--
-- Rendering is scaled down from 640dpi to prevent the need for upscaling given
-- the prevalence of XHDPI (320dpi) consumer devices.
--
-- Therefore all UI textures must be exported at 640dpi.
--
-- Consequently all UI geometry is calculated at 640dpi scale then scaled to
-- the pixel resolution of the display and rounded to the nearest pixel.
--
-- This means that UI space is the same scale as screen pixel space and we also
-- standardised the origin to the top left of the viewport to match graphics
-- packages in which UI design takes place.
--

maxVertices :: Int
maxVertices = 1024

defaultDpi :: Num a => a
defaultDpi = 240

textureDpi :: Num a => a
textureDpi = 640

zMax :: Num a => a
zMax = 1000

cardNineSliceTexturePath :: FilePath
cardNineSliceTexturePath = "assets/textures/ui/card-nine-slice.png"

buttonNineSliceTexurePath :: FilePath
buttonNineSliceTexurePath = "assets/textures/ui/button-nine-slice.png"

newtype UI = UI UIElem

data UIElem =
    UICard UIElem
  | UIButton String
{-
  | UILabel String
  | UIButton String
  | UIColumn [UIElem]
  | UIRow [UIElem]
-}


type Shader' os = CompiledShader os (ShaderEnv os)

data ShaderEnv os = ShaderEnv {
    shaderTexture :: Texture2D os (Format RGBAFloat),
    shaderPrimitives :: PrimitiveArray Triangles BVertex
  }

type BVertex = (B2 Float, B2 Float)


type Renderer ctx os m = ReaderT (RenderEnv os) (ContextT ctx os m)

runRenderer :: RenderEnv os -> Renderer ctx os m a -> ContextT ctx os m a
runRenderer = flip runReaderT

data RenderEnv os = RenderEnv {
    rendererButtonMaterial :: NineSlice os,
    rendererCardMaterial :: NineSlice os,
    rendererScale :: Float,
    rendererShader :: Shader' os,
    rendererVertexBuffer :: Buffer os BVertex,
    rendererViewportSize :: V2 Float
  }

liftContextT :: ContextT ctx os m a -> Renderer ctx os m a
liftContextT = ReaderT . const


createRenderer :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Window' os
  -> WindowSize
  -> ContextT ctx os m (UI -> ContextT ctx os m ())
createRenderer window windowSize@(vw, vh) = do
  -- Calculate screen DPI
  dpi <- liftIO . fmap (fromMaybe defaultDpi) . runMaybeT $ getDpi

  -- UI materials
  cardMaterial <- fromNineSlicePng cardNineSliceTexturePath
                    (P (V2 192 192), P (V2 212 212))
  buttonMaterial <- fromNineSlicePng buttonNineSliceTexurePath
                      (P (V2 252 0), P (V2 700 512))

  -- Shader environment
  --  Pre-allocated buffers
  vertexBuffer :: Buffer os BVertex <- newBuffer maxVertices
  shader <- createShader window windowSize

  let scale' = dpi / textureDpi

  let renderEnv = RenderEnv {
          rendererButtonMaterial = buttonMaterial,
          rendererCardMaterial = cardMaterial,
          rendererScale = scale',
          rendererShader = shader,
          rendererVertexBuffer = vertexBuffer,
          rendererViewportSize = V2 (fromIntegral vw / scale')
                                    (fromIntegral vh / scale')
        }

  -- SDF font sub-renderer
  renderText <- Text.initialise window windowSize

  font <- loadFont

  let text = createText font typefaceMain 32 (V2 1024 768) "Hello, World!"

  return $ \ui -> do
    runRenderer renderEnv $ do
      renderUi ui

    -- Render test text
    renderText 1 text

createShader :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Window' os
  -> WindowSize
  -> ContextT ctx os m (Shader' os)
createShader window (vw, vh) =
  compileShader $ do
    -- Textures
    let sampleFilter = SamplerFilter Linear Linear Nearest Nothing
    sampler <- newSampler2D $ \s ->
      (shaderTexture s, sampleFilter, (pure Repeat, undefined))

    -- Vertex shader
    primitiveStream <- toPrimitiveStream shaderPrimitives
    let primitiveStream' = flip fmap primitiveStream $
          \(V2 x y, uv) ->
             let vw' = fromIntegral vw
                 vh' = fromIntegral vh
             in (ortho 0 vw' 0 vh' 0 zMax !* V4 x (vh' - y) 0 1, uv)

    -- Fragment shader
    let shadeFragment = sample2D sampler SampleAuto Nothing Nothing

    fragmentStream <- fmap (fmap shadeFragment)
      . flip rasterize primitiveStream'
      . const $
          (Front,
           ViewPort (V2 0 0) (V2 vw vh),
           DepthRange 0 1
          )

    flip drawWindowColor fragmentStream $
      const (window, ContextColorOption blending (pure True))
 where
  blending =
    BlendRgbAlpha
      (FuncAdd, FuncAdd)
      (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors One Zero)
      0

renderUi :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m)
  => UI
  -> Renderer ctx os m ()
renderUi (UI el) = do
  V2 vw vh <- asks rendererViewportSize
  (V2 inW inH) <- elemSize el
  let x = (vw - inW) / 2
      y = (vh - inH) / 2
  renderElem (P $ V2 x y) el

elemInnerSize :: Monad m => UIElem -> Renderer ctx os m (V2 Float)
elemInnerSize (UIButton _ ) =
  -- Buttons should scale horizontally
  return $ V2 1000 512
elemInnerSize (UICard   el) = elemSize el

elemSize :: Monad m => UIElem -> Renderer ctx os m (V2 Float)
elemSize el = do
  NineSlice{..} <- case el of
    UIButton _ -> asks rendererButtonMaterial
    UICard _   -> asks rendererCardMaterial

  -- Border dimensions
  let V2 leftW  topH    = unP . fst $ nineSliceBoundaries
      V2 rightW bottomH = nineSliceSize - (unP . snd $ nineSliceBoundaries)

  innerSize <- elemInnerSize el

  return . (+ innerSize) . fmap fromIntegral
    $ V2 (leftW + rightW) (topH + bottomH)

renderElem :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m)
  => Point V2 Float
  -> UIElem
  -> Renderer ctx os m ()
renderElem (P orig) el = do
  scale' <- asks rendererScale

  -- Inner dimensions (at full scale)
  (V2 innerW innerH) <- elemInnerSize el

  NineSlice{..} <- case el of
    UIButton _ -> asks rendererButtonMaterial
    UICard _   -> asks rendererCardMaterial

  -- Border dimensions (at full scale)
  let V2 leftW  topH = unP . fmap fromIntegral . fst $ nineSliceBoundaries
      V2 rightW botH = fmap fromIntegral $ nineSliceSize - (unP . snd $ nineSliceBoundaries)

  -- Render nine slice quads
  --
  -- Top left border
  renderQuad (nineSlices V.! 0) scale' orig leftW topH
  -- Top border
  let topOff = orig + V2 leftW 0
  renderQuad (nineSlices V.! 1) scale' topOff innerW topH
  -- Top right border
  let topRightOff = orig + V2 (leftW + innerW) 0
  renderQuad (nineSlices V.! 2) scale' topRightOff rightW topH
  -- Left border
  let leftOff = orig + V2 0 topH
  renderQuad (nineSlices V.! 3) scale' leftOff leftW innerH
  -- Centre
  let centreOff = orig + V2 leftW topH
  renderQuad (nineSlices V.! 4) scale' centreOff innerW innerH
  -- Right border
  let rightOff = orig + V2 (leftW + innerW) topH
  renderQuad (nineSlices V.! 5) scale' rightOff rightW innerH
  -- Bottom left border
  let botLeftOff = orig + V2 0 (topH + innerH)
  renderQuad (nineSlices V.! 6) scale' botLeftOff leftW botH
  -- Bottom border
  let botOff = orig + V2 leftW (topH + innerH)
  renderQuad (nineSlices V.! 7) scale' botOff innerW botH
  -- Bttom right border
  let botRightOff = orig + V2 (leftW + innerW) (topH + innerH)
  renderQuad (nineSlices V.! 8) scale' botRightOff rightW botH

  -- Render children
  case el of
    UIButton _ -> return ()
    UICard el' -> renderElem (P $ orig + V2 leftW topH) el'

 where
  renderQuad :: Texture2D os (Format RGBAFloat)
    -> Float
    -> V2 Float
    -> Float
    -> Float
    -> Renderer ctx os m ()
  renderQuad texture scale' (V2 ox oy) w h  = do
    vertexBuffer <- asks rendererVertexBuffer
    shader <- asks rendererShader

    -- Screen space width, height and origin
    let w'  = roundFloat $ w * scale'
        h'  = roundFloat $ h * scale'
        ox' = roundFloat $ ox * scale'
        oy' = roundFloat $ oy * scale'

    -- Texture co-ordinates - we don't sample from the edges of the texture but
    -- from the middle of each pixel where it maps onto the original texture.
    let u0  = ((w / w') *       0.5)  / w
        v0  = ((h / h') *       0.5)  / h
        u1  = ((w / w') * (w' - 0.5)) / w
        v1  = ((h / h') * (h' - 0.5)) / h

    let vertTopL = (V2  ox'        oy'      , V2 u0 v0)
        vertTopR = (V2 (ox' + w')  oy'      , V2 u1 v0)
        vertBotL = (V2  ox'       (oy' + h'), V2 u0 v1)
        vertBotR = (V2 (ox' + w') (oy' + h'), V2 u1 v1)

    let vertices = [
            vertTopR, vertTopL, vertBotL,
            vertTopR, vertBotL, vertBotR
          ]

    liftContextT $ do
      writeBuffer vertexBuffer 0 vertices

      render $ do
        vertexArray <- fmap (takeVertices . length $ vertices)
                         . newVertexArray $ vertexBuffer
        let primitiveArray = toPrimitiveArray TriangleList vertexArray
            env = ShaderEnv {
                      shaderTexture = texture,
                      shaderPrimitives = primitiveArray
                    }
        shader env

getDpi :: MaybeT IO Float
getDpi = do
  monitor <- MaybeT GLFW.getPrimaryMonitor
  widthMm <- liftIO . GLFW.getMonitorPhysicalSize $ monitor
  videoMode <- MaybeT . GLFW.getVideoMode $ monitor
  let widthIn = mmToIn . fst $ widthMm
      widthPx = fromIntegral . GLFW.videoModeWidth $ videoMode
  return $ widthPx / widthIn
 where
  mmToIn :: Int -> Float
  mmToIn = (/ 25.4) . fromIntegral

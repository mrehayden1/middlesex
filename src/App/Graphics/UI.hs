module App.Graphics.UI (
  loadFont,

  typefaceIxLabel,
  typefaceIxBody,
  typefaceIxHeading,
  typefaceIxDebug,

  UI(..),
  UIElem(..),

  createUIText,

  createRenderer
) where

import Control.Lens
import Control.Monad
import Control.Monad.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.List as L
import Data.Maybe
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

newtype UI os = UI (UIElem os)

data UIElem os =
    UICard (UIElem os)
  | UIButton (Text os)
  | UILayoutColumn [UIElem os]
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

type Vertex  = (V2 Float, V2 Float)
type BVertex = (B2 Float, B2 Float)


type Renderer ctx os m = ReaderT (RenderEnv ctx os m) (ContextT ctx os m)

runRenderer :: RenderEnv ctx os m -> Renderer ctx os m a -> ContextT ctx os m a
runRenderer = flip runReaderT

data RenderEnv ctx os m = RenderEnv {
    rendererButtonMaterial :: NineSlice os,
    rendererCardMaterial :: NineSlice os,
    rendererScale :: Float,
    rendererShader :: Shader' os,
    rendererTextRenderer :: TextRenderer ctx os m,
    rendererVertexBuffer :: Buffer os BVertex,
    rendererViewportSize :: V2 Float
  }

liftContextT :: ContextT ctx os m a -> Renderer ctx os m a
liftContextT = ReaderT . const


createRenderer :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Window' os
  -> WindowSize
  -> ContextT ctx os m (UI os -> ContextT ctx os m ())
createRenderer window windowSize@(vw, vh) = do
  -- Calculate screen DPI
  dpi <- liftIO . fmap (fromMaybe defaultDpi) . runMaybeT $ getDpi

  -- UI materials
  cardMaterial <- fromNineSlicePng cardNineSliceTexturePath
                    (P (V2 192 192), P (V2 212 212))
  buttonMaterial <- fromNineSlicePng buttonNineSliceTexurePath
                      (P (V2 252 32), P (V2 700 416))

  -- Shader environment
  --  Pre-allocated buffers
  vertexBuffer :: Buffer os BVertex <- newBuffer maxVertices
  shader <- createShader window windowSize

  let scale' = dpi / textureDpi

  -- SDF font sub-renderer
  renderText <- Text.initialise window windowSize

  let renderEnv = RenderEnv {
          rendererButtonMaterial = buttonMaterial,
          rendererCardMaterial = cardMaterial,
          rendererScale = scale',
          rendererShader = shader,
          rendererTextRenderer = renderText,
          rendererVertexBuffer = vertexBuffer,
          rendererViewportSize = V2 (fromIntegral vw / scale')
                                    (fromIntegral vh / scale')
        }

  return $ \ui -> do
    runRenderer renderEnv $ do
      renderUi ui

createShader :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Window' os
  -> WindowSize
  -> ContextT ctx os m (Shader' os)
createShader window (vw, vh) =
  compileShader $ do
    -- Textures
    let sampleFilter = SamplerFilter Linear Linear Linear Nothing
    sampler <- newSampler2D $ \s ->
      (shaderTexture s, sampleFilter, (pure Repeat, undefined))

    -- Vertex shader
    primitiveStream <- toPrimitiveStream shaderPrimitives
    let primitiveStream' = flip fmap primitiveStream $ \(V2 x y, uv) ->
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
  => UI os
  -> Renderer ctx os m ()
renderUi (UI el) = do
  V2 vw vh <- asks rendererViewportSize
  (V2 inW inH) <- elemSize el
  let x = (vw - inW) / 2
      y = (vh - inH) / 2
  renderElem (P $ V2 x y) el

elemSize :: Monad m => UIElem os -> Renderer ctx os m (V2 Float)
elemSize (UIButton t) = do
  outerSz <- asks (nineSliceBorderSize . rendererButtonMaterial)
  innerSz <- buttonInnerSize t
  return $ innerSz + outerSz
elemSize (UICard e) = do
  outerSz <- asks (nineSliceBorderSize . rendererCardMaterial)
  innerSz <- cardInnerSize e
  return $ innerSz + outerSz
elemSize (UILayoutColumn es) = do
  sizes <- mapM elemSize es
  let width = L.foldl' (flip $ max . (^. _x)) 0 sizes
      height = L.foldl' (flip $ (+) . (^. _y)) 0 sizes
  return . V2 width $ height

cardInnerSize :: Monad m => UIElem os -> Renderer ctx os m (V2 Float)
cardInnerSize = elemSize

buttonInnerSize :: Monad m => Text os -> Renderer ctx os m (V2 Float)
buttonInnerSize t = do
  scale <- asks rendererScale
  -- Buttons only scale horizontally, i.e. they have a fixed height
  (P (V2 _ height)) <- asks (uncurry subtract . nineSliceBoundaries
                               . rendererButtonMaterial)
  return $ V2 (textWidth t / scale) (fromIntegral height)

renderElem :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m)
  => Point V2 Float
  -> UIElem os
  -> Renderer ctx os m ()

renderElem (P orig) (UIButton t) = do
  renderText <- asks rendererTextRenderer

  scale' <- asks rendererScale
  nineSlice <- asks rendererButtonMaterial

  -- Render button background
  inner@(V2 _ innerH) <- buttonInnerSize t
  renderNineSlice (P orig) inner nineSlice

  -- Offset
  let V2 leftW topH = unP . fmap fromIntegral . fst . nineSliceBoundaries
                        $ nineSlice
      textHeight'   = textHeight t / scale'
      offsetV       = topH + (innerH - textHeight') / 2

  -- Render children
  liftContextT . renderText t . P $ (orig + V2 leftW offsetV) ^* scale'

renderElem (P orig) (UICard el) = do
  nineSlice <- asks rendererCardMaterial

  -- Render card background
  inner <- cardInnerSize el
  renderNineSlice (P orig) inner nineSlice

  -- Offset
  let V2 leftW topH = unP . fmap fromIntegral . fst . nineSliceBoundaries
                        $ nineSlice

  -- Render children
  renderElem (P $ orig + V2 leftW topH) el

renderElem (P orig) (UILayoutColumn es) = do
  foldM_ accum orig es
 where
  accum orig' elem' = do
    renderElem (P orig') elem'
    V2 _ height <- elemSize elem'
    return $ orig' + V2 0 height


renderNineSlice :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m)
  => Point V2 Float
  -> V2 Float
  -> NineSlice os
  -> Renderer ctx os m ()
renderNineSlice (P orig) (V2 innerW innerH) NineSlice{..} = do
  vertexBuffer <- asks rendererVertexBuffer
  shader <- asks rendererShader

  scale' <- asks rendererScale

  -- Sprite border slice dimensions
  let V2 leftW  topH = unP . fmap fromIntegral . fst $ nineSliceBoundaries
      V2 rightW botH = fmap fromIntegral
                         $ nineSliceSize - (unP . snd $ nineSliceBoundaries)

  let (V2 w h) = nineSliceSize
      (P (V2 x0 y0), P (V2 x1 y1)) = nineSliceBoundaries

  let (vertices, numVertices) = execWriter $ do
        -- Top left border
        makeQuad scale' orig leftW topH (V2 0 0) (V2 x0 y0)
        -- Top border
        let topOff = orig + V2 leftW 0
        makeQuad scale' topOff innerW topH (V2 x0 0) (V2 x1 y0)
        -- Top right border
        let topRightOff = orig + V2 (leftW + innerW) 0
        makeQuad scale' topRightOff rightW topH (V2 x1 0) (V2 w y0)
        -- Left border
        let leftOff = orig + V2 0 topH
        makeQuad scale' leftOff leftW innerH (V2 0 y0) (V2 x0 y1)
        -- Centre
        let centreOff = orig + V2 leftW topH
        makeQuad scale' centreOff innerW innerH (V2 x0 y0) (V2 x1 y1)
        -- Right border
        let rightOff = orig + V2 (leftW + innerW) topH
        makeQuad scale' rightOff rightW innerH (V2 x1 y0) (V2 w y1)
        -- Bottom left border
        let botLeftOff = orig + V2 0 (topH + innerH)
        makeQuad scale' botLeftOff leftW botH (V2 0 y1) (V2 x0 h)
        -- Bottom border
        let botOff = orig + V2 leftW (topH + innerH)
        makeQuad scale' botOff innerW botH (V2 x0 y1) (V2 x1 h)
        -- Bottom right border
        let botRightOff = orig + V2 (leftW + innerW) (topH + innerH)
        makeQuad scale' botRightOff rightW botH (V2 x1 y1) (V2 w h)

  liftContextT $ do
    writeBuffer vertexBuffer 0 vertices

    render $ do
      vertexArray <- fmap (takeVertices . getSum $ numVertices)
                       . newVertexArray $ vertexBuffer
      let primitiveArray = toPrimitiveArray TriangleList vertexArray
          env = ShaderEnv {
                    shaderTexture = nineSliceTexture,
                    shaderPrimitives = primitiveArray
                  }
      shader env
 where
  makeQuad :: Float
    -> V2 Float
    -> Float
    -> Float
    -> V2 Int
    -> V2 Int
    -> Writer ([Vertex], Sum Int) ()
  makeQuad scale' (V2 ox oy) w h (V2 tx0 ty0) (V2 tx1 ty1) = do
    -- Screen space width, height and origin
    let w'  = w  * scale'
        h'  = h  * scale'
        ox' = ox * scale'
        oy' = oy * scale'

    -- Texture co-ordinates
    -- Note that we don't sample from the very edges of the texture but from the
    -- middle of each pixel where they map onto the sprite.
    -- Texture size
    let (V2 tw th) = nineSliceSize
        tw'  = fromIntegral tw
        th'  = fromIntegral th
    -- Texture pixel co-ordinates
    let tx0' = fromIntegral tx0
        tx1' = fromIntegral tx1
        ty0' = fromIntegral ty0
        ty1' = fromIntegral ty1
        -- Slice width and height
    let sw'  = tx1' - tx0'
        sh'  = ty1' - ty0'

    let u0  = (tx0' + ((sw' / w') *       0.5))  / tw'
        v0  = (ty0' + ((sh' / h') *       0.5))  / th'
        u1  = (tx0' + ((sw' / w') * (w' - 0.5))) / tw'
        v1  = (ty0' + ((sh' / h') * (h' - 0.5))) / th'

    let topL = (roundFloat <$> V2  ox'        oy'      , V2 u0 v0)
        topR = (roundFloat <$> V2 (ox' + w')  oy'      , V2 u1 v0)
        botL = (roundFloat <$> V2  ox'       (oy' + h'), V2 u0 v1)
        botR = (roundFloat <$> V2 (ox' + w') (oy' + h'), V2 u1 v1)

    let vertices = [
            topR, topL, botL,
            topR, botL, botR
          ]

    tell (vertices, Sum 6)

    return ()


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

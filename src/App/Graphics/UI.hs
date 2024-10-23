module App.Graphics.UI (
  loadFont,

  typefaceIxLabel,
  typefaceIxBody,
  typefaceIxHeading,
  typefaceIxDebug,

  UI(..),
  UIElem(..),
  UIMaterial(..),

  uiSdfText,

  createRenderer,

  UIElemID,
  UIElemIDTexture,
  readUiElemIdTexture,
  readUiElemIdTexturePixel
) where

import Control.Lens
import Control.Monad
import Control.Monad.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Word
import Graphics.GPipe
import qualified Graphics.UI.GLFW as GLFW
import Linear.Affine

import App.Graphics.Text hiding (Renderer)
import qualified App.Graphics.Text as Text
import App.Graphics.Texture
import App.Graphics.Window
import App.Math

-- Important Notes on UI Rendering
--
-- Rendering is scaled down from 640dpi to prevent the need for upscaling given
-- the prevalence of XHDPI (320dpi) consumer devices.
--
-- This means all UI textures must be exported at 640dpi.
--
-- Consequently all UI is specified at 640dpi then scaled so that the physical
-- size remains the same at the resolution of the display and also rounded to
-- the nearest pixel.


-- UI material texture paths
bannerNineSliceTexurePath :: FilePath
bannerNineSliceTexurePath = "assets/textures/ui/banner-nine-slice.png"

buttonNineSliceTexurePath :: FilePath
buttonNineSliceTexurePath = "assets/textures/ui/button-nine-slice.png"

cardNineSliceTexturePath :: FilePath
cardNineSliceTexturePath = "assets/textures/ui/card-nine-slice.png"


maxVertices :: Int
maxVertices = 1024

defaultDpi :: Num a => a
defaultDpi = 240

textureDpi :: Num a => a
textureDpi = 640

zMax :: Num a => a
zMax = 1000


-- UI Container
-- Centralises the element on the screen
-- TODO: Add other anchors, e.g. bottom, top right, etc.
newtype UI os = UI [UIElem os]


data UIElem os =
    UINineSlice UIMaterial ScaleX ScaleY UIElemID [UIElem os]
  | UIText (SDFText os)

type ScaleX = Bool
type ScaleY = Bool

-- UI Materials
data UIMaterial =
    UIMaterialBanner
  | UIMaterialButton
  | UIMaterialCard

data Materials os = Materials {
    materialBanner :: NineSlice os,
    materialButton :: NineSlice os,
    materialCard   :: NineSlice os
  }

loadMaterials :: (ContextHandler ctx, MonadIO m)
  => ContextT ctx os m (Materials os)
loadMaterials = do
  -- Load UI materials
  Materials
    <$> fromNineSlicePng bannerNineSliceTexurePath 616 830 0   704
    <*> fromNineSlicePng buttonNineSliceTexurePath 252 700 32  416
    <*> fromNineSlicePng cardNineSliceTexturePath  192 212 192 212

-- Element ID texture, used for object picking
type UIElemID = Word32

type UIElemIDTexture os = Texture2D os (Format RWord)
type UIElemIDImage = Image (Format RWord)

readUiElemIdTexture :: (ContextHandler ctx, MonadAsyncException m)
  => UIElemIDTexture os
  -> ContextT ctx os m (Map (Int, Int) UIElemID)
readUiElemIdTexture tex = do
  let sz@(V2 w h) = head . texture2DSizes $ tex
  let coords = [ (x, y) | x <- [1..w], y <- [1..h] ]
  pixels :: [UIElemID] <- readTexture2D tex 0 0 sz ((return .) . flip (:)) []
  return . M.fromList . filter ((/= 0) . snd) . zip coords $ pixels

readUiElemIdTexturePixel :: (ContextHandler ctx, MonadAsyncException m)
  => UIElemIDTexture os
  -> Int
  -> Int
  -> ContextT ctx os m UIElemID
readUiElemIdTexturePixel tex x y =
  readPixelTexture2D tex 0 (V2 x y)


type Shader' os = CompiledShader os (ShaderEnv os)

data ShaderEnv os = ShaderEnv {
    shaderElemIdImage :: UIElemIDImage,
    shaderMaterialTexture :: Texture2D os (Format RGBAFloat),
    shaderPrimitives :: PrimitiveArray Triangles BVertex
  }


--              Position  Texture uv  Element ID
type Vertex  = (V2 Float, V2 Float  , UIElemID)
type BVertex = (B2 Float, B2 Float  , B UIElemID)

type Renderer ctx os m = ReaderT (RenderEnv ctx os m) (ContextT ctx os m)

data RenderEnv ctx os m = RenderEnv {
    rendererElemIdTexture :: UIElemIDTexture os,
    rendererMaterials :: Materials os,
    rendererScale :: Float,
    rendererShader :: Shader' os,
    rendererTextRenderer :: Text.Renderer ctx os m,
    rendererVertexBuffer :: Buffer os BVertex,
    rendererViewportSize :: V2 Float
  }

runRenderer :: RenderEnv ctx os m -> Renderer ctx os m a -> ContextT ctx os m a
runRenderer = flip runReaderT

liftContextT :: ContextT ctx os m a -> Renderer ctx os m a
liftContextT = ReaderT . const

getMaterial :: Monad m => UIMaterial -> Renderer ctx os m (NineSlice os)
getMaterial material = do
  Materials{..} <- asks rendererMaterials

  return $ case material of
    UIMaterialBanner -> materialBanner
    UIMaterialButton -> materialButton
    UIMaterialCard   -> materialCard

-- Create a UI renderer and element ID texture.
--
-- The renderer renders the UI to the back buffer and all the interactable
-- element IDs to the texture.
createRenderer :: (ContextHandler ctx, MonadAsyncException m)
  => Window' os
  -> WindowSize
  -> ContextT ctx os m (UI os -> ContextT ctx os m (), UIElemIDTexture os)
createRenderer window windowSize@(vw, vh) = do

  -- Calculate screen DPI and UI scale
  dpi <- liftIO . fmap (fromMaybe defaultDpi) . runMaybeT $ getDpi
  let scale' = dpi / textureDpi

  -- Load materials
  materials <- loadMaterials

  -- Pre-allocated buffers
  vertexBuffer :: Buffer os BVertex <- newBuffer maxVertices

  -- SDF font sub-renderer
  renderText <- Text.initialise window windowSize

  -- Single image, for element picking
  elemIdTexture :: UIElemIDTexture os <- newTexture2D R32UI (V2 vw vh) 1

  shader <- createShader window windowSize

  let renderEnv = RenderEnv {
          rendererElemIdTexture = elemIdTexture,
          rendererMaterials = materials,
          rendererScale = scale',
          rendererShader = shader,
          rendererTextRenderer = renderText,
          rendererVertexBuffer = vertexBuffer,
          rendererViewportSize = V2 (fromIntegral vw / scale')
                                    (fromIntegral vh / scale')
        }

  let doRender ui = do
        -- Clear the element ID texture
        render $ do
          elemIdImage <- getTexture2DImage elemIdTexture 0
          clearImageColor elemIdImage 0

        runRenderer renderEnv $ do
          renderUi ui

  return (doRender, elemIdTexture)

createShader :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Window' os
  -> WindowSize
  -> ContextT ctx os m (Shader' os)
createShader window (vw, vh) =
  compileShader $ do
    -- Textures
    let sampleFilter = SamplerFilter Linear Linear Linear Nothing
    sampler <- newSampler2D $ \s ->
      (shaderMaterialTexture s, sampleFilter, (pure Repeat, undefined))

    -- Vertex shader
    primitiveStream <- toPrimitiveStream shaderPrimitives
    let primitiveStream' = flip fmap primitiveStream $ \(V2 x y, uv, i) ->
          let vw' = fromIntegral vw
              vh' = fromIntegral vh
              pos = ortho 0 vw' 0 vh' 0 zMax !* V4 x y 0 1
          in (pos, (uv, i))

    -- Fragment shader
    let shadeFragment = sample2D sampler SampleAuto Nothing Nothing

    fragmentStream <- flip rasterize primitiveStream'
      . const $
          (FrontAndBack,
           ViewPort (V2 0 0) (V2 vw vh),
           DepthRange 0 1
          )

    let colourFragments = fmap (shadeFragment . fst) fragmentStream
        elemIdFragments = fmap snd fragmentStream

    -- Draw depth and colour to the window back buffer
    flip drawWindowColor colourFragments $
      const (window, ContextColorOption blending (pure True))

    -- Draw element ID to the element ID texture
    draw (const NoBlending) elemIdFragments
      . drawColor $ \s -> (shaderElemIdImage s, True, False)
 where
  blending =
    BlendRgbAlpha
      (FuncAdd, FuncAdd)
      (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors One Zero)
      0

renderUi :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m)
  => UI os
  -> Renderer ctx os m ()
renderUi (UI elems) = do
  V2 vw vh <- asks rendererViewportSize
  V2 inW inH <- columnLayoutSize elems
  let x = (vw - inW) / 2
      y = (vh - inH) / 2

  renderElemsColumn (P (V2 x y)) elems

renderElem :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m)
  => Point V2 Float
  -> UIElem os
  -> Renderer ctx os m ()

renderElem (P orig) (UINineSlice material scaleX scaleY elemId elems) = do
  nineSlice <- getMaterial material

  innerSz@(V2 innerWidth innerHeight)
    <- nineSliceInnerSize nineSlice scaleX scaleY elems

  -- Render the material (background texture)
  renderNineSliceMaterial elemId (P orig) innerSz nineSlice

  -- Compute the offset for the children
  V2 contentWidth contentHeight <- columnLayoutSize elems
  let (P bottomLeft) = fst . nineSliceBoundaries $ nineSlice
      innerOffset = P $ orig + fmap fromIntegral bottomLeft
        + V2 ((innerWidth  - contentWidth)  / 2)
             ((innerHeight - contentHeight) / 2)

  -- Render children
  renderElemsColumn innerOffset elems

renderElem orig (UIText t) = do
  renderText <- asks rendererTextRenderer

  scale' <- asks rendererScale

  liftContextT . renderText t (orig ^* scale') $ scale'
  return ()


renderElemsColumn :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Point V2 Float
  -> [UIElem os]
  -> Renderer ctx os m ()
renderElemsColumn (P orig) elems = do
  V2 width _ <- columnLayoutSize elems
  foldM_ (accum width) orig elems
 where
  accum colWidth orig'@(V2 ox oy) elem' = do
    V2 elemWidth elemHeight <- elemSize elem'
    let xOffset = (colWidth - elemWidth) / 2
    renderElem (P (V2 (ox + xOffset) oy)) elem'
    return $ orig' + V2 0 elemHeight


renderNineSliceMaterial :: forall ctx os m. (
    ContextHandler ctx,
    MonadIO m,
    MonadException m
  )
  => UIElemID
  -> Point V2 Float
  -> V2 Float
  -> NineSlice os
  -> Renderer ctx os m ()
renderNineSliceMaterial elemId (P orig) (V2 innerW innerH) NineSlice{..}
  = do
  vertexBuffer <- asks rendererVertexBuffer
  shader <- asks rendererShader
  elemIdTexture <- asks rendererElemIdTexture

  scale' <- asks rendererScale

  -- Sprite border slice dimensions
  let V2 leftW  botH = unP . fmap fromIntegral . fst $ nineSliceBoundaries
      V2 rightW topH = fmap fromIntegral
                         $ nineSliceSize - (unP . snd $ nineSliceBoundaries)

  let (V2 w h) = nineSliceSize
      (P (V2 x0 y0), P (V2 x1 y1)) = nineSliceBoundaries

  let (vertices, numVertices) = execWriter $ do
        -- Bottom left border
        makeQuad scale' orig leftW botH (V2 0 0) (V2 x0 y0)
        -- Bottom border
        let botOff = orig + V2 leftW 0
        makeQuad scale' botOff innerW botH (V2 x0 0) (V2 x1 y0)
        -- Bottom right border
        let botRightOff = orig + V2 (leftW + innerW) 0
        makeQuad scale' botRightOff rightW botH (V2 x1 0) (V2 w y0)
        -- Left border
        let leftOff = orig + V2 0 botH
        makeQuad scale' leftOff leftW innerH (V2 0 y0) (V2 x0 y1)
        -- Centre
        let centreOff = orig + V2 leftW botH
        makeQuad scale' centreOff innerW innerH (V2 x0 y0) (V2 x1 y1)
        -- Right border
        let rightOff = orig + V2 (leftW + innerW) botH
        makeQuad scale' rightOff rightW innerH (V2 x1 y0) (V2 w y1)
        -- Top left border
        let topLeftOff = orig + V2 0 (botH + innerH)
        makeQuad scale' topLeftOff leftW topH (V2 0 y1) (V2 x0 h)
        -- Top border
        let topOff = orig + V2 leftW (botH + innerH)
        makeQuad scale' topOff innerW topH (V2 x0 y1) (V2 x1 h)
        -- Top right border
        let topRightOff = orig + V2 (leftW + innerW) (botH + innerH)
        makeQuad scale' topRightOff rightW topH (V2 x1 y1) (V2 w h)

  liftContextT $ do
    writeBuffer vertexBuffer 0 vertices

    render $ do
      vertexArray <- fmap (takeVertices . getSum $ numVertices)
                       . newVertexArray $ vertexBuffer

      elemIdImage <- getTexture2DImage elemIdTexture 0

      let primitiveArray = toPrimitiveArray TriangleList vertexArray
          env = ShaderEnv {
                    shaderElemIdImage = elemIdImage,
                    shaderMaterialTexture = nineSliceTexture,
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
    -- Note that we don't sample from the very edges of the texture but from
    -- the middle of each pixel where they map onto the sprite.
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

    let topL = (roundFloat <$> V2  ox'        oy'      , V2 u0 v0, elemId)
        topR = (roundFloat <$> V2 (ox' + w')  oy'      , V2 u1 v0, elemId)
        botL = (roundFloat <$> V2  ox'       (oy' + h'), V2 u0 v1, elemId)
        botR = (roundFloat <$> V2 (ox' + w') (oy' + h'), V2 u1 v1, elemId)

    let vertices = [
            topR, topL, botL,
            topR, botL, botR
          ]

    tell (vertices, Sum 6)

    return ()


elemSize :: Monad m => UIElem os -> Renderer ctx os m (V2 Float)
elemSize (UINineSlice material scaleX scaleY _ elems) = do
  nineSlice <- getMaterial material
  let outerSz = nineSliceBorderSize nineSlice
  innerSz <- nineSliceInnerSize nineSlice scaleX scaleY elems
  return $ innerSz + outerSz
elemSize (UIText t) = return $ V2 <$> sdfTextWidth <*> sdfTextHeight $ t


nineSliceInnerSize :: Monad m
  => NineSlice os
  -> ScaleX
  -> ScaleY
  -> [UIElem os]
  -> Renderer ctx os m (V2 Float)
nineSliceInnerSize nineSlice scaleX scaleY elems' = do
  V2 contentWidth contentHeight <- columnLayoutSize elems'
  let P (V2 sliceWidth sliceHeight) = uncurry subtract
                                        . nineSliceBoundaries $ nineSlice
  let innerWidth = if scaleX
                     then contentWidth
                     else fromIntegral sliceWidth
      innerHeight = if scaleY
                      then contentHeight
                      else fromIntegral sliceHeight
  return $ V2 innerWidth innerHeight


columnLayoutSize :: Monad m => [UIElem os] -> Renderer ctx os m (V2 Float)
columnLayoutSize elems = do
  sizes <- mapM elemSize elems
  let width = L.foldl' (flip $ max . (^. _x)) 0 sizes
      height = L.foldl' (flip $ (+) . (^. _y)) 0 sizes
  return $ V2 width height


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

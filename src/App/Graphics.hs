module App.Graphics (
  module App.Graphics.Camera,
  module App.Graphics.Projection,
  module Linear,

  Scene,

  initialise,

  ContextT,
  runContextT,

  Handle,
  defaultHandleConfig,

  swapWindowBuffers,

  windowShouldClose,

  setKeyCallback,
  Key(..),
  KeyState(..),
  ModifierKeys(..),

  setCursorPosCallback,

  setMouseButtonCallback,
  MouseButton(..),
  MouseButtonState(..)
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Map
import Data.Maybe
import Graphics.GPipe
import qualified Graphics.GPipe as GPipe
import Graphics.GPipe.Context.GLFW
import qualified Graphics.UI.GLFW as GLFW
import Linear

import App.Graphics.Board as Board
import App.Graphics.Board.Tile as Tile
import App.Graphics.Camera
import App.Graphics.Env
import qualified App.Graphics.Text as Text
import App.Graphics.Projection

defaultDpi :: Int
defaultDpi = 240

maxInstances :: Int
maxInstances = 1024

fullscreen :: Bool
--fullscreen = true
fullscreen = false

windowHeight, windowWidth :: Maybe Int
windowHeight = Just 1080
windowWidth = Just 1920

--windowHeight = Just 1440
--windowWidth = Just 2560

--windowHeight = Nothing
--windowWidth = Nothing

type WindowSize = (Int, Int)

-- When no window size can be determined, either from the setting above or the
-- primary monitor's current video mode, we default to these values.
windowHeightFallback, windowWidthFallback :: Int
windowHeightFallback = 1080
windowWidthFallback = 1920

initialise :: (ctx ~ Handle, MonadIO m, MonadException m)
  => String
  -> ContextT ctx os m (
         Window' os,
         WindowSize,
         Scene -> ContextT ctx os m ()
       )
initialise name = do
  mMonitor <- liftIO GLFW.getPrimaryMonitor
  mVideoMode <- liftIO . fmap join . mapM GLFW.getVideoMode $ mMonitor

  -- Calculate the DPI
  dpi <- liftIO . fmap (fromMaybe defaultDpi) . runMaybeT $ getDpi

  liftIO . print $ dpi

  let windowHeight' = fromMaybe windowHeightFallback $
                        windowHeight <|> fmap GLFW.videoModeHeight mVideoMode
      windowWidth'  = fromMaybe windowWidthFallback $
                        windowWidth <|> fmap GLFW.videoModeWidth mVideoMode

      windowSize    = (windowWidth', windowHeight')

  let windowConfig = WindowConfig {
          configHeight = windowHeight',
          configHints = [GLFW.WindowHint'Samples (Just 8)],
          -- Setting a monitor runs the app in fullscreen.
          configMonitor = if fullscreen then mMonitor else Nothing,
          configSwapInterval = Just 0,
          configTitle = name,
          configWidth = windowWidth'
        }

  window <- newWindow (WindowFormatColor RGBA8) windowConfig

  renderScene <- createSceneRenderer window windowSize

  return (window, windowSize, renderScene)

getDpi :: MaybeT IO Int
getDpi = do
  monitor <- MaybeT GLFW.getPrimaryMonitor
  widthMm <- liftIO . GLFW.getMonitorPhysicalSize $ monitor
  videoMode <- MaybeT . GLFW.getVideoMode $ monitor
  let widthIn = mmToIn . fst $ widthMm
      widthPx = fromIntegral . GLFW.videoModeWidth $ videoMode
  return . round $ widthPx / widthIn
 where
  mmToIn :: Int -> Float
  mmToIn = (/ 25.4) . fromIntegral

createSceneRenderer :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Window' os
  -> WindowSize
  -> ContextT ctx os m (Scene -> ContextT ctx os m ())
createSceneRenderer window windowSize = do
  shader <- createShader window windowSize

  renderText <- Text.initialise window

  (mapModel, tileModels) <- Board.makeModels

  -- Preallocate buffers
  instanceBuffer :: Buffer os (V4 (B4 Float)) <- newBuffer maxInstances
  viewMatrixBuffer :: Buffer os (Uniform (V4 (B4 Float))) <- newBuffer 1
  baseColourBuffer :: Buffer os (Uniform (B4 Float)) <- newBuffer 1

  let renderEnv = RenderEnv {
        rendererBuffers = RenderBuffers {
            renderBufferBaseColour = baseColourBuffer,
            renderBufferInstances = instanceBuffer,
            renderBufferViewMatrix = viewMatrixBuffer
          },
        rendererShader = shader
      }

  return $ \Scene{..} -> do
    -- Clear the colour buffer
    GPipe.render $ clearWindowColor window 0

    -- Camera view matrix
    let viewM = toViewMatrix sceneCamera

    -- Render the scene
    runRenderer renderEnv $ do
      -- Render the map background
      renderInstances mapModel viewM (Board.mapInstances sceneBoard) 1

      -- Render the board tiles
      let tileInstances = Tile.instances sceneBoard
      forM_ (assocs tileInstances) $ \(tile, instances') -> do
        let model = tileModels ! tile
        renderInstances model viewM instances' . length $ instances'
      -- Render tile selection

    --renderText 0 1 (V2 960 540) "Hello, World!"

createShader :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Window' os
  -> WindowSize
  -> ContextT ctx os m (Shader' os)
createShader window (vw, vh) = do
  compileShader $ do
    -- Textures
    let sampleFilter = SamplerFilter Linear Linear Linear (Just 3)
    sampler <- newSampler2D $ \s ->
      (shaderAlbedoTexture s, sampleFilter, (pure Repeat, undefined))

    -- Matrices
    viewM <- getUniform ((, 0) . shaderViewMatrix)

    -- Base colour
    baseColour <- getUniform ((, 0) . shaderBaseColour)

    -- Vertex shader
    primitiveStream <- toPrimitiveStream shaderPrimitives
    let primitiveStream' = flip fmap primitiveStream $
          \((V3 x y z, uv), modelM) ->
             (projection vw vh !*! viewM !*! modelM !* V4 x y z 1, uv)

    -- Fragment shader
    let shadeFragment =
          (* baseColour) . sample2D sampler SampleAuto Nothing Nothing

    fragmentStream <- fmap (fmap shadeFragment)
      . flip rasterize primitiveStream'
      . const $
          (FrontAndBack,
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

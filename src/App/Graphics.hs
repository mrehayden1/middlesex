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
import Data.Maybe
import Graphics.GPipe
import qualified Graphics.GPipe as GPipe
import Graphics.GPipe.Context.GLFW
import qualified Graphics.UI.GLFW as GLFW
import Linear

import App.Graphics.Board
import App.Graphics.Camera
import App.Graphics.Env
import qualified App.Graphics.Text as Text
import App.Graphics.Projection

type Scene = Camera Float

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

createShader :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Window' os
  -> WindowSize
  -> ContextT ctx os m (Shader' os)
createShader window (vw, vh) = do
  compileShader $ do
    -- Textures
    let sampleFilter = SamplerFilter Linear Linear Linear (Just 3)
    sampler <- newSampler2D $ \s ->
      (shaderEnvAlbedoTexture s, sampleFilter, (pure Repeat, undefined))

    -- Matrices
    viewM <- getUniform (\s -> (shaderEnvViewMatrix s, 0))

    -- Base colour
    baseColour <- getUniform (\s -> (shaderEnvBaseColour s, 0))

    -- Vertex shader
    primitiveStream <- toPrimitiveStream shaderEnvPrimitives
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

initialise :: (ctx ~ Handle, MonadIO m, MonadException m)
  => String
  -> ContextT ctx os m (
         Window' os,
         Scene -> ContextT ctx os m (),
         WindowSize
       )
initialise name = do
  monitor <- liftIO GLFW.getPrimaryMonitor
  videoMode <- liftIO . fmap join . mapM GLFW.getVideoMode $ monitor

  let windowHeight' = fromMaybe windowHeightFallback $
                        windowHeight <|> fmap GLFW.videoModeHeight videoMode
      windowWidth'  = fromMaybe windowWidthFallback $
                        windowWidth <|> fmap GLFW.videoModeWidth videoMode

      windowSize    = (windowWidth', windowHeight')

  let windowConfig = WindowConfig {
          configHeight = windowHeight',
          configHints = [],
          -- Setting a monitor runs the app in fullscreen.
          configMonitor = if fullscreen then monitor else Nothing,
          configSwapInterval = Just 0,
          configTitle = name,
          configWidth = windowWidth'
        }

  window <- newWindow (WindowFormatColor RGBA8) windowConfig

  shader <- createShader window windowSize

  renderText <- Text.initialise window

  (mapModel, tileInstances) <- makeModels

  instanceBuffer :: Buffer os (V4 (B4 Float)) <- newBuffer maxInstances

  let renderScene camera = do
        -- Clear the colour buffer
        GPipe.render $ clearWindowColor window 0

        -- Camera view matrix
        let viewM = toViewMatrix camera

        -- Render the scene
        runRenderer shader $ do
          -- Render the map background
          liftContextT . writeBuffer instanceBuffer 0 $ [identity]
          runShader mapModel viewM instanceBuffer 1
          -- Render the map tiles
          forM_ tileInstances $ \(model, instances) -> do
            liftContextT . writeBuffer instanceBuffer 0 $ instances
            runShader model viewM instanceBuffer . length $ instances
        --renderText 0 1 (V2 960 540) "Hello, World!"

  return (window, renderScene, windowSize)

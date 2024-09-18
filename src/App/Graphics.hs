module App.Graphics (
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

  setCursorPosCallback
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Exception
import Control.Monad.IO.Class
import Data.Maybe
import Graphics.GPipe hiding (Render, Shader, render)
import qualified Graphics.GPipe as GPipe
import Graphics.GPipe.Context.GLFW
import qualified Graphics.UI.GLFW as GLFW
import Linear

import App.Graphics.Env
import qualified App.Graphics.Text as Text
import App.Graphics.Texture
import App.Matrix

type Scene = V2 Float

data ShaderEnv os = ShaderEnv {
    shaderEnvBaseColour :: V4 Float,
    shaderEnvAlbedoTexture :: Texture2D os (Format RGBAFloat),
    shaderEnvModelM :: Buffer os (Uniform (V4 (B4 Float))),
    shaderEnvViewM :: Buffer os (Uniform (V4 (B4 Float))),
    shaderEnvGeometry :: PrimitiveArray Triangles (B2 Float, B2 Float)
  }

type Shader os = CompiledShader os (ShaderEnv os)

type Renderer ctx os m = ContextT ctx os m ()

tileFilePath :: FilePath
tileFilePath = "assets/textures/hextiles/grass1.png"

fullscreen :: Bool
fullscreen = false

-- When no window size can be determined, either from the override or the
-- primary monitor's current video mode, we default to these values.
windowHeightDefault, windowWidthDefault :: Int
windowHeightDefault = 1080
windowWidthDefault = 1920

windowHeightOverride, windowWidthOverride :: Maybe Int
windowHeightOverride = Just 1080
windowWidthOverride = Just 1920
--windowHeightOverride = Nothing
--windowWidthOverride = Nothing

createShader :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Window' os
  -> ContextT ctx os m (Shader os)
createShader window = do
  V2 vw vh <- getFrameBufferSize window
  let vw' = realToFrac vw
      vh' = realToFrac vh

  compileShader $ do
    let sampleFilter = SamplerFilter Linear Linear Linear (Just 2)
    sampler <- newSampler2D $ \s ->
      (shaderEnvAlbedoTexture s, sampleFilter, (pure Repeat, undefined))

    modelM <- getUniform (\s -> (shaderEnvModelM s, 0))
    viewM <- getUniform (\s -> (shaderEnvViewM s, 0))

    let fov = 20 -- degrees
        projM = perspective (fov * pi / 180) (vw'/vh') 0.001 10000

    primitiveStream <- toPrimitiveStream shaderEnvGeometry
    let primitiveStream' = flip fmap primitiveStream $
          \(V2 x y, uv) -> (projM !*! viewM !*! modelM !* V4 x y 0 1, uv)

    fragmentStream <- fmap (fmap (sample2D sampler SampleAuto Nothing Nothing))
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
  -> ContextT ctx os m (Window' os, Scene -> Renderer ctx os m, (Int, Int))
initialise name = do
  monitor <- liftIO GLFW.getPrimaryMonitor
  videoMode <- liftIO . fmap join . mapM GLFW.getVideoMode $ monitor

  let windowHeight = fromMaybe windowHeightFallback $
                       windowHeightOverride
                         <|> fmap GLFW.videoModeHeight videoMode
      windowWidth = fromMaybe windowWidthFallback $
                      windowWidthOverride
                        <|> fmap GLFW.videoModeWidth videoMode

  let windowConfig = WindowConfig {
          configHeight = windowHeight,
          configHints = [],
          -- Setting a monitor runs the app in fullscreen.
          configMonitor = if fullscreen then monitor else Nothing,
          configSwapInterval = Just 0,
          configTitle = name,
          configWidth = windowWidth
        }

  window <- newWindow (WindowFormatColor RGBA8) windowConfig

  shader <- createShader window

  -- Buffer textures
  tileTexture <- fromPNG tileFilePath

  renderText <- Text.initialise window

  let render scene = do
        GPipe.render $ clearWindowColor window 1
        renderGrid shader tileTexture scene
        --renderText 0 1 (V2 960 540) "Hello, World!"

  return (window, render, (windowWidth, windowHeight))

renderGrid :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Shader os
  -> Texture2D os (Format RGBAFloat)
  -> Scene
  -> Renderer ctx os m
renderGrid shader tileTexture (V2 x z) = do
  -- Buffer vertices
  let vertices = makeTileVertices 10 10 1
  vertexBuffer <- newBuffer . length $ vertices
  writeBuffer vertexBuffer 0 vertices

  -- Buffer uniforms
  modelMBuffer :: Buffer os (Uniform (V4 (B4 Float))) <- newBuffer 1
  writeBuffer modelMBuffer 0 [rotateX (-pi / 2)]

  let camera = Camera {
          camPitch = -(3 * pi) / 8,
          camPos   = V3 x 0 z + V3 0 25 10,
          camYaw   = pi / 2
        }
  viewMBuffer :: Buffer os (Uniform (V4 (B4 Float))) <- newBuffer 1
  writeBuffer viewMBuffer 0 [toViewMatrix camera]

  GPipe.render $ do
    vertexArray <- newVertexArray vertexBuffer
    let primitiveArray = toPrimitiveArray TriangleList vertexArray

    shader $ ShaderEnv {
        shaderEnvBaseColour = 1,
        shaderEnvAlbedoTexture = tileTexture,
        shaderEnvModelM = modelMBuffer,
        shaderEnvViewM = viewMBuffer,
        shaderEnvGeometry = primitiveArray
      }

makeTileVertices :: Int -> Int -> Float -> [(V2 Float, V2 Float)]
makeTileVertices nx ny size =
  let coords = makeGridCoords nx ny
  in concatMap (hexTileVertices size . offset) coords
 where
  offset (V2 x y) = V2 (xOffset x y) (yOffset y)

  xOffset x y | even y    =  fromIntegral (x - nx `div` 2)        * sqrt 3 * size
              | otherwise = (fromIntegral (x - nx `div` 2) + 0.5) * sqrt 3 * size

  yOffset y = fromIntegral (y - ny `div` 2) * 3 * size / 2

-- Creates a list of hex-grid offset coordinates for the given width and height.
-- See https://www.redblobgames.com/grids/hexagons/ for more.
makeGridCoords :: Int -> Int -> [V2 Int]
makeGridCoords w h = do
  y <- [0..(h - 1)]
  x <- [0..(w - 1)]
  guard $ even y || x /= w - 1
  return $ V2 x y

-- Create vertices (positions and UVs) of the regular hexagon where size is
-- the shortest distance from the centre to any point.
hexTileVertices :: Float -> V2 Float -> [(V2 Float, V2 Float)]
hexTileVertices size orig =
  [c, ne, n ,
   c, n , nw,
   c, nw, sw,
   c, sw, s ,
   c, s , se,
   c, se, ne
  ]
 where
  --        position               u    v
  -- Hexagon centre
  c  = (orig, V2 0.5  0.5 )
  -- The corners of the hexagon
  ne = (hexNorthEast size orig, V2 1    0.25)
  n  = (hexNorth     size orig, V2 0.5  0   )
  nw = (hexNorthWest size orig, V2 0    0.25)
  se = (hexSouthEast size orig, V2 1    0.75)
  s  = (hexSouth     size orig, V2 0.5  1   )
  sw = (hexSouthWest size orig, V2 0    0.75)

hexNorthEast :: Float -> V2 Float -> V2 Float
hexNorthEast size (V2 origX origY) = V2 (origX + w / 2) (origY + size / 2)
 where
  w = size * sqrt 3

hexNorth :: Float -> V2 Float -> V2 Float
hexNorth size (V2 origX origY) = V2 origX (origY + size)

hexNorthWest :: Float -> V2 Float -> V2 Float
hexNorthWest size (V2 origX origY) = V2 (origX - w / 2) (origY + size / 2)
 where
  w = size * sqrt 3

hexSouthEast :: Float -> V2 Float -> V2 Float
hexSouthEast size (V2 origX origY) = V2 (origX + w / 2) (origY - size / 2)
 where
  w = size * sqrt 3

hexSouth :: Float -> V2 Float -> V2 Float
hexSouth size (V2 origX origY) = V2 origX (origY - size)

hexSouthWest :: Float -> V2 Float -> V2 Float
hexSouthWest size (V2 origX origY) = V2 (origX - w / 2) (origY - size / 2)
 where
  w = size * sqrt 3

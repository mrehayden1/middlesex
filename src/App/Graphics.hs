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
import Control.Arrow
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
    shaderEnvBaseColour :: Buffer os (Uniform (B4 Float)),
    shaderEnvAlbedoTexture :: Texture2D os (Format RGBAFloat),
    shaderEnvModelM :: Buffer os (Uniform (V4 (B4 Float))),
    shaderEnvViewM :: Buffer os (Uniform (V4 (B4 Float))),
    shaderEnvGeometry :: PrimitiveArray Triangles (B3 Float, B2 Float)
  }

type Shader os = CompiledShader os (ShaderEnv os)

type Renderer ctx os m = ContextT ctx os m ()

tileFilePath :: FilePath
tileFilePath = "assets/textures/hextiles/grass1.png"

mapTexturePath :: FilePath
mapTexturePath = "assets/textures/map.png"

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
    -- Textures
    let sampleFilter = SamplerFilter Linear Linear Linear (Just 3)
    sampler <- newSampler2D $ \s ->
      (shaderEnvAlbedoTexture s, sampleFilter, (pure Repeat, undefined))

    -- Matrices
    modelM <- getUniform (\s -> (shaderEnvModelM s, 0))
    viewM <- getUniform (\s -> (shaderEnvViewM s, 0))

    let fov = 20 -- degrees
        projM = perspective (fov * pi / 180) (vw'/vh') 0.001 10000
    -- Base colour
    baseColour <- getUniform (\s -> (shaderEnvBaseColour s, 0))

    -- Vertex shader
    primitiveStream <- toPrimitiveStream shaderEnvGeometry
    let primitiveStream' = flip fmap primitiveStream $
          \(V2 x y, uv) -> (projM !*! viewM !*! modelM !* V4 x y 0 1, uv)

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

  renderText <- Text.initialise window

  -- Tile data
  tileTexture <- fromPng tileFilePath

  let hexGridWidth = 30
      hexGridHeight = 21

      tileVertices = fmap (first (\(V2 x y) -> V3 x y 0))
        . makeTileVertices hexGridWidth hexGridHeight
        $ 1

      tileBaseColour = V4 0 0 0 1
      tileMaterial = Material tileBaseColour tileTexture

      tileModelMatrix = rotateX (-pi / 2)

      tilesModel = Model tileMaterial tileModelMatrix tileVertices

  -- Map data
  mapTexture <- fromPng mapTexturePath

  let mapVertices = fmap (first (\(V2 x y) -> V3 x y 0))
        . makeMapVertices hexGridWidth hexGridHeight 1
        $ 1

      mapBaseColour = V4 1 1 1 1
      mapMaterial = Material mapBaseColour mapTexture

      mapModelMatrix = rotateX (-pi / 2)

      mapModel = Model mapMaterial mapModelMatrix mapVertices

  let renderScene (V2 camX camZ) = do
        -- Clear the colour buffer
        GPipe.render $ clearWindowColor window 0

        -- Camera view matrix
        let camera = Camera {
                camPitch = -(3 * pi) / 8,
                camPos   = V3 camX 0 camZ + V3 0 25 10,
                camYaw   = pi / 2
              }
            viewM = toViewMatrix camera

        -- Render the scene
        render shader viewM mapModel
        render shader viewM tilesModel
        --renderText 0 1 (V2 960 540) "Hello, World!"

  return (window, renderScene, (windowWidth, windowHeight))

data Model os = Model {
    modelMaterial :: Material os,
    modelMatrix :: ModelM,
    modelVertices :: [(V3 Float, V2 Float)]
  }

data Material os = Material {
    materialBaseColour :: V4 Float,
    materialAlbedoTexture :: Texture2D os (Format RGBAFloat)
  }

type ViewM = M44 Float

type ModelM = M44 Float

render :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Shader os
  -> ViewM
  -> Model os
  -> Renderer ctx os m
render shader viewM Model{..} = do
  let Material{..} = modelMaterial

  -- Vertex buffer
  vertexBuffer <- newBuffer . length $ modelVertices
  writeBuffer vertexBuffer 0 modelVertices

  -- Uniforms
  --   Model matrix
  modelMBuffer :: Buffer os (Uniform (V4 (B4 Float))) <- newBuffer 1
  writeBuffer modelMBuffer 0 [modelMatrix]

  --   View matrix
  viewMBuffer :: Buffer os (Uniform (V4 (B4 Float))) <- newBuffer 1
  writeBuffer viewMBuffer 0 [viewM]

  --   Base colour
  baseColour :: Buffer os (Uniform (B4 Float)) <- newBuffer 1
  writeBuffer baseColour 0 [materialBaseColour]

  GPipe.render $ do
    vertexArray <- newVertexArray vertexBuffer
    let primitiveArray = toPrimitiveArray TriangleList vertexArray

    shader $ ShaderEnv {
        shaderEnvBaseColour = baseColour,
        shaderEnvAlbedoTexture = materialAlbedoTexture,
        shaderEnvModelM = modelMBuffer,
        shaderEnvViewM = viewMBuffer,
        shaderEnvGeometry = primitiveArray
      }

-- Make the geometry of an nx by ny pointy-top oriented hextile board where the
-- origin is in the middle of the board and the even rows are offset by one.
makeTileVertices :: Int -> Int -> Float -> [(V2 Float, V2 Float)]
makeTileVertices nx ny size =
  let coords = makeGridCoords nx ny
  in concatMap (hexTileVertices size . offset) coords
 where
  offset (V2 x y) = V2 (xOffset x y) (yOffset y)

  xOffset x y | even y    = (fromIntegral (x - nx `div` 2) + 0.5) * sqrt 3 * size
              | otherwise = (fromIntegral (x - nx `div` 2) + 1  ) * sqrt 3 * size

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

-- Make the geometry of a background for an nx by ny hextile board.
makeMapVertices :: Int -> Int -> Float -> Float -> [(V2 Float, V2 Float)]
makeMapVertices nx ny size padding =
  let sw = (V2 ((-w / 2) - padding) ((-h / 2) - padding), V2 0 1)
      se = (V2 (( w / 2) + padding) ((-h / 2) - padding), V2 1 1)
      ne = (V2 (( w / 2) + padding) (( h / 2) + padding), V2 1 0)
      nw = (V2 ((-w / 2) - padding) (( h / 2) + padding), V2 0 0)
      w  = size * sqrt 3 * fromIntegral nx
      h  = (3 * fromIntegral ny * size) / 2 + (size / 2)
  in [sw, se, nw, se, ne, nw]

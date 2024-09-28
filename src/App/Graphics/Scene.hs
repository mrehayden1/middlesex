module App.Graphics.Scene (
  Renderer,
  createSceneRenderer,

  Scene(..),
) where

import Control.Arrow
import Control.Monad.Exception
import Control.Monad.Reader
import Data.Map
import Graphics.GPipe

import App.Game.Board
import App.Graphics.Camera
import App.Graphics.Projection
import App.Graphics.Scene.Board as Board
import App.Graphics.Scene.Board.Tile as Tile
import App.Graphics.Scene.Model
import App.Graphics.Window

maxInstances :: Int
maxInstances = 1024

type Shader' os = CompiledShader os (ShaderEnv os)

data ShaderEnv os = ShaderEnv {
    shaderAlbedoTexture :: Texture2D os (Format RGBAFloat),
    shaderPrimitives :: PrimitiveArray Triangles (BVertex, ModelMatrix B4),
    shaderUniforms :: Buffer os (Uniform UniformsB)
  }

data Uniforms = Uniforms {
    baseColour :: V4 Float,
    viewMatrix :: M44 Float
  }

data UniformsB = UniformsB {
    baseColourB :: B4 Float,
    viewMatrixB :: V4 (B4 Float)
  }

data UniformsS x = UniformsS {
    baseColourS :: V4 (S x Float),
    viewMatrixS :: M44 (S x Float)
  }

instance BufferFormat UniformsB where
  type HostFormat UniformsB = Uniforms
  toBuffer = proc ~(Uniforms{..}) -> do
               baseColour' <- toBuffer -< baseColour
               viewMatrix' <- toBuffer -< viewMatrix
               returnA -< UniformsB {
                              baseColourB = baseColour',
                              viewMatrixB = viewMatrix'
                            }

instance UniformInput UniformsB where
  type UniformFormat UniformsB x = UniformsS x
  toUniform = proc ~(UniformsB{..}) -> do
                baseColour' <- toUniform -< baseColourB
                viewMatrix' <- toUniform -< viewMatrixB
                returnA -< UniformsS {
                               baseColourS = baseColour',
                               viewMatrixS = viewMatrix'
                             }

type Renderer ctx os m = ReaderT (RenderEnv os) (ContextT ctx os m)

runRenderer :: RenderEnv os -> Renderer ctx os m a -> ContextT ctx os m a
runRenderer = flip runReaderT

data RenderEnv os = RenderEnv {
    rendererBuffers :: RenderBuffers os,
    rendererShader :: Shader' os
  }

-- Pre allocated buffers for uniforms, instances, etc
data RenderBuffers os = RenderBuffers {
    renderBufferInstances :: Buffer os (V4 (B4 Float)),
    renderBufferUniforms :: Buffer os (Uniform UniformsB)
  }


liftContextT :: ContextT ctx os m a -> Renderer ctx os m a
liftContextT = ReaderT . const


type ViewMatrix = M44 Float

type ModelMatrix v = V4 (v Float)

data Scene = Scene {
    sceneBoard :: Board,
    sceneCamera :: Camera Float,
    sceneSelection :: Maybe (Int, Int)
  }

createSceneRenderer :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Window' os
  -> WindowSize
  -> ContextT ctx os m (Scene -> ContextT ctx os m ())
createSceneRenderer window windowSize = do
  shader <- createShader window windowSize

  (mapModel, tileModels) <- Board.makeModels

  -- Preallocate buffers
  instanceBuffer :: Buffer os (V4 (B4 Float)) <- newBuffer maxInstances
  uniformsBuffer :: Buffer os (Uniform UniformsB) <- newBuffer 1

  let renderEnv = RenderEnv {
        rendererBuffers = RenderBuffers {
            renderBufferInstances = instanceBuffer,
            renderBufferUniforms = uniformsBuffer
          },
        rendererShader = shader
      }

  return $ \Scene{..} -> do
    -- Clear the colour buffer
    render $ clearWindowColor window 0

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

-- Render `n` instances of the given model with the shader in the current
-- environment. We do it this way so we can pre-allocate the instance buffer
-- and continuously write to it.
renderInstances :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Model os
  -> ViewMatrix
  -> [M44 Float]
  -> Int
  -> Renderer ctx os m ()
renderInstances Model{..} viewM instances' n = do
  shader <- asks rendererShader
  RenderBuffers{..} <- asks rendererBuffers

  liftContextT $ do
    -- Buffer uniforms
    let Material{..} = modelMaterial

    let shaderUniforms = Uniforms {
            baseColour = materialBaseColour,
            viewMatrix = viewM
          }

    writeBuffer renderBufferUniforms 0 [shaderUniforms]

    -- Buffer instances
    writeBuffer renderBufferInstances 0 instances'

    render $ do
      vertexArray <- newVertexArray modelVertices
      instancesArray <- fmap (takeVertices n) . newVertexArray
                          $ renderBufferInstances
      let primitiveArray = toPrimitiveArrayInstanced modelPrimitiveTopology (,)
                             vertexArray instancesArray
          env = ShaderEnv {
                    shaderAlbedoTexture = materialAlbedoTexture,
                    shaderPrimitives = primitiveArray,
                    shaderUniforms = renderBufferUniforms
                  }
      shader env

  return ()

createShader :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Window' os
  -> WindowSize
  -> ContextT ctx os m (Shader' os)
createShader window (vw, vh) =
  compileShader $ do
    -- Textures
    let sampleFilter = SamplerFilter Linear Linear Linear (Just 3)
    sampler <- newSampler2D $ \s ->
      (shaderAlbedoTexture s, sampleFilter, (pure Repeat, undefined))

    -- Uniforms
    vertexUniforms <- getUniform ((, 0) . shaderUniforms)
    fragmentUniforms <- getUniform ((, 0) . shaderUniforms)

    --  Matrices
    let viewMatrix = viewMatrixS vertexUniforms
    --  Base colour
        baseColour = baseColourS fragmentUniforms

    -- Vertex shader
    primitiveStream <- toPrimitiveStream shaderPrimitives
    let primitiveStream' = flip fmap primitiveStream $
          \((V3 x y z, uv), modelMatrix) ->
             (projection vw vh !*! viewMatrix !*! modelMatrix !* V4 x y z 1, uv)

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

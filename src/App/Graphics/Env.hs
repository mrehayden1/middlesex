module App.Graphics.Env (
  Window',

  Renderer,
  RenderEnv(..),
  RenderBuffers(..),
  runRenderer,

  liftContextT,

  Shader',
  ShaderEnv(..),
  UniformsB,
  UniformsS(..),

  renderInstances,

  ViewMatrix,

  Model(..),

  Vertex,
  BVertex,

  Material(..),

  Scene(..),
) where

import Control.Arrow
import Control.Monad.Exception
import Control.Monad.Reader
import Graphics.GPipe

import App.Game.Board
import App.Graphics.Camera


type Window' os = Window os RGBAFloat ()


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

type Vertex = (V3 Float, V2 Float)
type BVertex = (B3 Float, B2 Float)


data Scene = Scene {
    sceneBoard :: Board,
    sceneCamera :: Camera Float,
    sceneSelection :: Maybe (Int, Int)
  }

data Model os = Model {
    modelMaterial :: Material os,
    modelPrimitiveTopology :: PrimitiveTopology Triangles,
    modelVertices :: Buffer os BVertex
  }

data Material os = Material {
    materialAlbedoTexture :: Texture2D os (Format RGBAFloat),
    materialBaseColour :: V4 Float
  }

-- Render `n` instances of the given model with the shader in the current
-- environment. We do it this way so we can pre-allocate the instance buffer
-- and continuously write to it.
renderInstances :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Model os
  -> ViewMatrix
  -> [M44 Float]
  -> Int
  -> Renderer ctx os m ()
renderInstances Model{..} viewM instances n = do
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
    writeBuffer renderBufferInstances 0 instances

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

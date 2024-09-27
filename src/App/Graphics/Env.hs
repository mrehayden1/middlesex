module App.Graphics.Env (
  Window',

  Renderer,
  RenderEnv(..),
  RenderBuffers(..),
  runRenderer,

  liftContextT,

  Shader',
  ShaderEnv(..),

  renderInstances,

  ViewMatrix,

  Model(..),

  Vertex,
  BVertex,

  Material(..),

  Scene(..),
) where

import Control.Monad.Exception
import Control.Monad.Reader
import Graphics.GPipe

import App.Game.Board
import App.Graphics.Camera


type Window' os = Window os RGBAFloat ()


type Shader' os = CompiledShader os (ShaderEnv os)

data ShaderEnv os = ShaderEnv {
    shaderAlbedoTexture :: Texture2D os (Format RGBAFloat),
    shaderBaseColour :: Buffer os (Uniform (B4 Float)),
    shaderPrimitives :: PrimitiveArray Triangles (BVertex, ModelMatrix B4),
    shaderViewMatrix :: Buffer os (Uniform (V4 (B4 Float)))
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
    renderBufferBaseColour :: Buffer os (Uniform (B4 Float)),
    renderBufferInstances :: Buffer os (V4 (B4 Float)),
    renderBufferViewMatrix :: Buffer os (Uniform (V4 (B4 Float)))
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
    writeBuffer renderBufferInstances 0 instances

    let Material{..} = modelMaterial

    writeBuffer renderBufferBaseColour 0 [materialBaseColour]
    writeBuffer renderBufferViewMatrix 0 [viewM]
    writeBuffer renderBufferInstances 0 instances

    render $ do
      vertexArray <- newVertexArray modelVertices
      instancesArray <- fmap (takeVertices n) . newVertexArray
                          $ renderBufferInstances
      let primitiveArray = toPrimitiveArrayInstanced modelPrimitiveTopology (,)
                             vertexArray instancesArray
          env = ShaderEnv {
                    shaderAlbedoTexture = materialAlbedoTexture,
                    shaderBaseColour = renderBufferBaseColour,
                    shaderPrimitives = primitiveArray,
                    shaderViewMatrix = renderBufferViewMatrix
                  }
      shader env

  return ()

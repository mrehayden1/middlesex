module App.Graphics.Env (
  Renderer,
  runRenderer,

  liftContextT,

  Shader',
  ShaderEnv(..),
  runShader,

  ViewMatrix,

  Model(..),

  Vertex,
  BVertex,

  Material(..),

  Window'
) where

import App.Graphics.Texture
import Control.Monad.Exception
import Control.Monad.Reader

import Graphics.GPipe

type Window' os = Window os RGBAFloat ()

type Shader' os = CompiledShader os (ShaderEnv os)


newtype Renderer ctx os m a = Renderer {
    unRenderer :: ReaderT (Shader' os, ShaderEnv os) (ContextT ctx os m) a
  } deriving (Applicative, Functor, Monad)

liftContextT :: Monad m => ContextT ctx os m a -> Renderer ctx os m a
liftContextT = Renderer . lift

getEnv :: Monad m => Renderer ctx os m (ShaderEnv os)
getEnv = Renderer . asks $ snd

getShader :: Monad m => Renderer ctx os m (Shader' os)
getShader = Renderer . asks $ fst


type ViewMatrix = M44 Float

type ModelMatrix v = V4 (v Float)

type Vertex = (V3 Float, V2 Float)
type BVertex = (B3 Float, B2 Float)

type InstanceBuffer os = Buffer os (V4 (B4 Float))

data Model os = Model {
    modelMaterial :: Material os,
    modelPrimitiveTopology :: PrimitiveTopology Triangles,
    modelVertices :: Buffer os BVertex
  }

data Material os = Material {
    materialAlbedoTexture :: Texture2D os (Format RGBAFloat),
    materialBaseColour :: V4 Float
  }

data ShaderEnv os = ShaderEnv {
    shaderEnvAlbedoTexture :: Texture2D os (Format RGBAFloat),
    shaderEnvBaseColour :: Buffer os (Uniform (B4 Float)),
    shaderEnvPrimitives :: PrimitiveArray Triangles (BVertex, ModelMatrix B4),
    shaderEnvViewMatrix :: Buffer os (Uniform (V4 (B4 Float)))
  }

runRenderer :: (ContextHandler ctx, MonadIO m)
  => CompiledShader os (ShaderEnv os)
  -> Renderer ctx os m a
  -> ContextT ctx os m a
runRenderer shader renderer = do
  -- View matrix
  viewMBuffer :: Buffer os (Uniform (V4 (B4 Float))) <- newBuffer 1
  -- Albedo
  baseColour :: Buffer os (Uniform (B4 Float)) <- newBuffer 1
  defaultAlbedoTexture <- makeDefaultAlbedoTexture

  let env = ShaderEnv {
          shaderEnvAlbedoTexture = defaultAlbedoTexture,
          shaderEnvBaseColour = baseColour,
          shaderEnvPrimitives = undefined,
          shaderEnvViewMatrix = viewMBuffer
        }

  flip runReaderT (shader, env) . unRenderer $ renderer

-- Render `n` instances of the given model with the shader in the current
-- environment. We do it this way so we can pre-allocate the instance buffer
-- and continuously write to it.
runShader :: (ContextHandler ctx, MonadIO m, MonadException m)
  => Model os
  -> ViewMatrix
  -> InstanceBuffer os
  -> Int
  -> Renderer ctx os m ()
runShader Model{..} viewM instances n = do
  env@ShaderEnv{..} <- getEnv
  shader <- getShader

  let Material{..} = modelMaterial

  liftContextT . writeBuffer shaderEnvBaseColour 0 $ [materialBaseColour]
  liftContextT . writeBuffer shaderEnvViewMatrix 0 $ [viewM]

  liftContextT . render $ do
    vertexArray <- newVertexArray modelVertices
    instancesArray <- fmap (takeVertices n) . newVertexArray $ instances
    let primitiveArray = toPrimitiveArrayInstanced modelPrimitiveTopology (,)
                           vertexArray instancesArray
        env' = env {
                   shaderEnvAlbedoTexture = materialAlbedoTexture,
                   shaderEnvPrimitives = primitiveArray
                 }
    shader env'

  return ()

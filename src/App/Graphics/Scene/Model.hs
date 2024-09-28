module App.Graphics.Scene.Model (
  Model(..),

  Vertex,
  BVertex,

  Material(..)
) where

import Graphics.GPipe

data Model os = Model {
    modelMaterial :: Material os,
    modelPrimitiveTopology :: PrimitiveTopology Triangles,
    modelVertices :: Buffer os BVertex
  }

type Vertex = (V3 Float, V2 Float)
type BVertex = (B3 Float, B2 Float)

data Material os = Material {
    materialAlbedoTexture :: Texture2D os (Format RGBAFloat),
    materialBaseColour :: V4 Float
  }

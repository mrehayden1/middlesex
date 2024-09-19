module App.Graphics.Texture (
  fromJpeg,
  fromPng,

  makeDefaultAlbedoTexture
) where

import Data.ByteString
import Data.Word
import Codec.Picture.Jpg
import Codec.Picture.Png
import Codec.Picture.Types
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Graphics.GPipe hiding (Image)

fromJpeg :: (ContextHandler ctx, MonadIO m)
  => FilePath
  -> ContextT ctx os m (Texture2D os (Format RGBAFloat))
fromJpeg = fromImage decodeJpeg

fromPng :: (ContextHandler ctx, MonadIO m)
  => FilePath
  -> ContextT ctx os m (Texture2D os (Format RGBAFloat))
fromPng = fromImage decodePng

fromImage :: (ContextHandler ctx, MonadIO m)
  => (ByteString -> Either String DynamicImage)
  -> FilePath
  -> ContextT ctx os m (Texture2D os (Format RGBAFloat))
fromImage decode filePath = do
  bytes <- liftIO . BS.readFile $ filePath
  let image@Image{..} = either error fromDynamicImage . decode $ bytes
      sz@(V2 w h)     = V2 imageWidth imageHeight

      pixels = [
          pixelToV4 . pixelAt image i $ j
        | j <- [0..(h - 1)], i <- [0..(w - 1)]
        ]

  t <- newTexture2D RGBA8 sz maxBound
  writeTexture2D t 0 0 sz pixels
  generateTexture2DMipmap t
  return t

 where
  fromDynamicImage dynamicImage =
    case dynamicImage of
      ImageRGB8   img -> pixelMap promotePixel img
      ImageRGBA8  img -> img
      ImageYCbCr8 img -> pixelMap (promotePixel @PixelRGB8 . convertPixel) img
      _               -> error $ "fromImage: expecting 24-bit true color or"
                          ++ " 24-bit true color + 8-bit transparent image."

-- A 1 pixel x 1 pixel white texture for use in material shaders which require
-- an albedo texture.
makeDefaultAlbedoTexture :: (ContextHandler ctx, MonadIO m)
  => ContextT ctx os m (Texture2D os (Format RGBAFloat))
makeDefaultAlbedoTexture = do
  t <- newTexture2D RGBA8 (V2 1 1) maxBound
  writeTexture2D t 0 0 (V2 1 1) [V4 255 255 255 255 :: V4 Word8]
  return t

pixelToV4 :: PixelRGBA8 -> V4 Word8
pixelToV4 (PixelRGBA8 r g b a) = V4 r g b a

module App.Graphics.Texture (
  fromPNG
) where

import Data.Word
import Codec.Picture.Png
import Codec.Picture.Types
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Graphics.GPipe hiding (Image)

fromPNG :: (ContextHandler ctx, MonadIO m)
  => FilePath
  -> ContextT ctx os m (Texture2D os (Format RGBAFloat))
fromPNG filePath = do
  pngBytes <- liftIO . BS.readFile $ filePath

  let image@Image{..} = either error fromDynamicImage . decodePng $ pngBytes
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
      ImageRGB8  img -> pixelMap promotePixel img
      ImageRGBA8 img -> img
      _              -> error $ "fromPNG: expecting 24-bit true color or"
                          ++ " 24-bit true color + 8-bit transparent PNG."

pixelToV4 :: PixelRGBA8 -> V4 Word8
pixelToV4 (PixelRGBA8 r g b a) = V4 r g b a

module App.Graphics.Texture (
  fromPNG
) where

import Codec.Picture.Png
import Codec.Picture.Types
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Graphics.GPipe hiding (Image)

fromPNG :: (ContextHandler ctx, MonadIO m)
  => FilePath
  -> ContextT ctx os m (Texture2D os (Format RGBFloat))
fromPNG filePath = do
  pngBytes <- liftIO . BS.readFile $ filePath

  let image@Image{..} = either error fromImageRGB8 . decodePng $ pngBytes
      sz@(V2 w h)     = V2 imageWidth imageHeight

      pixels = [
          pixelToV3 . pixelAt image i $ j
        | j <- [0..(h - 1)], i <- [0..(w - 1)]
        ]

  t <- newTexture2D RGB8 sz 1
  writeTexture2D t 0 0 sz pixels

  return t

 where
  fromImageRGB8 dynamicImage =
    case dynamicImage of
      ImageRGB8 img -> img
      _             -> error "loadFont: expecting 24-bit true color image."

  pixelToV3 (PixelRGB8 r g b) = V3 r g b

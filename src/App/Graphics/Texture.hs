module App.Graphics.Texture (
  fromJpeg,
  fromPng,

  NineSlice(..),
  fromNineSlicePng,

  makeDefaultAlbedoTexture
) where

import Codec.Picture.Jpg
import Codec.Picture.Png
import Codec.Picture.Types
import Control.Monad.IO.Class
import Data.ByteString
import qualified Data.Vector.Fixed as V
import Data.Vector.Fixed.Boxed
import Data.Word
import qualified Data.ByteString as BS
import Graphics.GPipe hiding (Image)
import Linear.Affine

fromJpeg :: (ContextHandler ctx, MonadIO m)
  => FilePath
  -> ContextT ctx os m (Texture2D os (Format RGBAFloat))
fromJpeg = fromImageWithDecoder decodeJpeg

fromPng :: (ContextHandler ctx, MonadIO m)
  => FilePath
  -> ContextT ctx os m (Texture2D os (Format RGBAFloat))
fromPng = fromImageWithDecoder decodePng

fromImageWithDecoder :: (ContextHandler ctx, MonadIO m)
  => (ByteString -> Either String DynamicImage)
  -> FilePath
  -> ContextT ctx os m (Texture2D os (Format RGBAFloat))
fromImageWithDecoder decode filePath = do
  bytes <- liftIO . BS.readFile $ filePath
  let image@Image{..} = either error rgba8ImagefromDynamicImage . decode
                          $ bytes
      sz@(V2 w h)     = V2 imageWidth imageHeight

      pixels = [
          pixelToV4 . pixelAt image i $ j
        | j <- [0..(h - 1)], i <- [0..(w - 1)]
        ]

  t <- newTexture2D SRGB8A8 sz maxBound
  writeTexture2D t 0 0 sz pixels
  generateTexture2DMipmap t
  return t


rgba8ImagefromDynamicImage :: DynamicImage -> Image PixelRGBA8
rgba8ImagefromDynamicImage dynamicImage =
    case dynamicImage of
      ImageRGB8   img -> pixelMap promotePixel img
      ImageRGBA8  img -> img
      ImageYCbCr8 img -> pixelMap (promotePixel @PixelRGB8 . convertPixel) img
      _               -> error $ "fromImage: expecting 24-bit true color or"
                          ++ " 24-bit true color + 8-bit transparent image."

data NineSlice os = NineSlice {
    nineSlices :: Vec 9 (Texture2D os (Format RGBAFloat)),
    nineSliceBoundaries :: (Point V2 Int, Point V2 Int),
    nineSliceSize :: V2 Int
  }

fromNineSlicePng :: (ContextHandler ctx, MonadIO m)
  => FilePath
  -> (Point V2 Int, Point V2 Int)
  -> ContextT ctx os m (NineSlice os)
fromNineSlicePng = fromNineSliceImageWithDecoder decodePng

-- Given two points which represent the boundaries of the nine slices of the
-- sprite texture, create a nine slice sprite.
-- NB: The two pixel co-ordinates should be zero-based and represent the upper
-- left.
fromNineSliceImageWithDecoder :: (ContextHandler ctx, MonadIO m)
  => (ByteString -> Either String DynamicImage)
  -> FilePath
  -> (Point V2 Int, Point V2 Int)
  -> ContextT ctx os m (NineSlice os)
fromNineSliceImageWithDecoder decode filePath centre = do
  bytes <- liftIO . BS.readFile $ filePath
  let (p0, p1) = centre
      P (V2 x0 y0) = p0
      P (V2 x1 y1) = p1
      image@Image{..} = either error rgba8ImagefromDynamicImage . decode $ bytes
      sz@(V2 w h)     = V2 imageWidth imageHeight
      sliceBounds = V.mkN [sliceBounds]
        (P (V2  0  0), P (V2 (x0 - 1) (y0 - 1))) -- Top right
        (P (V2 x0  0), P (V2 (x1 - 1) (y0 - 1))) -- Top
        (P (V2 x1  0), P (V2 (w  - 1) (y0 - 1))) -- Top left
        (P (V2  0 y0), P (V2 (x0 - 1) (y1 - 1))) -- Left
        (P (V2 x0 y0), P (V2 (x1 - 1) (y1 - 1))) -- Centre
        (P (V2 x1 y0), P (V2 (w  - 1) (y1 - 1))) -- Right
        (P (V2  0 y1), P (V2 (x0 - 1) (h  - 1))) -- Bottom left
        (P (V2 x0 y1), P (V2 (x1 - 1) (h  - 1))) -- Bottom
        (P (V2 x1 y1), P (V2 (w  - 1) (h  - 1))) -- Bottom right
  textures <- mapM (fromImageSlice' image) sliceBounds
  return . NineSlice textures centre $ sz

fromImageSlice' :: (ContextHandler ctx, MonadIO m)
  => Image PixelRGBA8
  -> (Point V2 Int, Point V2 Int)
  -> ContextT ctx os m (Texture2D os (Format RGBAFloat))
fromImageSlice' image (p0@(P (V2 x0 y0)), p1@(P (V2 x1 y1))) = do
  let sz = unP . (+1) . subtract p0 $ p1
      pixels = [
            pixelToV4 . pixelAt image i $ j
          | j <- [y0..y1], i <- [x0..x1]
          ]
  t <- newTexture2D SRGB8A8 sz maxBound
  writeTexture2D t 0 0 sz pixels
  generateTexture2DMipmap t
  return t


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

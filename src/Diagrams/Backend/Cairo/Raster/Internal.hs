{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-|
Module      : Diagrams.Backend.Cairo.Raster.Internal
Description : Low-level primitives for Diagrams Cairo raster support.
Copyright   : (c) Taru Karttunen, 2014
License     : BSD3
Maintainer  : taruti@taruti.net
Stability   : experimental

Low-level primitives for Diagrams Cairo raster support.
-}
module Diagrams.Backend.Cairo.Raster.Internal(
  -- * High-level API
  cairoRaster, cairoLoadImage,
  -- * Colors
  CairoColor,
  crgb, crgba, crgbap,
  -- * Low-level API
  CairoSurface(..),
  dynamicImageToCairo,
  cairoBitmapArray,
  cairoSurfaceImage
  ) where

import qualified Codec.Picture                   as JC
import qualified Codec.Picture.Types             as JC
import           Data.Array.Base                 (MArray (unsafeWrite))
import           Data.Bits                       (Bits (..))
import           Data.Typeable                   (Typeable)
import           Data.Word                       (Word,Word8,Word32)
import           Diagrams.Backend.Cairo.Internal
import           Diagrams.Prelude
import qualified Graphics.Rendering.Cairo        as C


-- | An ARGB color as understood by Cairo. Cairo colors are with pre-multiplied alpha in a native endian format with the bytes from low to high: blue, green, red and alpha.
type CairoColor = Word32

{-# SPECIALISE INLINE crgb :: Int -> Int -> Int -> CairoColor #-}
{-# SPECIALISE INLINE crgb :: Word8 -> Word8 -> Word8 -> CairoColor #-}
-- | Construct a 'CairoColor' from red, green and blue between [0 .. 255].
crgb :: Integral a => a -> a -> a -> CairoColor
crgb !r !g !b = 0xFF000000 .|. w r 16 .|. w g 8 .|. w b 0
  where w x s = fromIntegral x `unsafeShiftL` s

{-# SPECIALISE INLINE crgba :: Int -> Int -> Int -> Int -> CairoColor #-}
{-# SPECIALISE INLINE crgba :: Word8 -> Word8 -> Word8 -> Word8 -> CairoColor #-}
-- | Construct a 'CairoColor' from non-premultiplied red, green, blue and alpha between [0 .. 255].
-- Non-premultiplied colors means that translucent red is @crgba 255 0 0 127@.
crgba :: Integral a => a -> a -> a -> a -> CairoColor
crgba !r !g !b !a = fromIntegral ((fromIntegral a `unsafeShiftL` 24) .|. ar .|. ag .|. ab)
  where alpha :: Word
        alpha = fromIntegral a
        amultTL16 :: Word -> Word
        amultTL16 c = ((257 * c * alpha)+257) .&. 0xFF0000
        ar = amultTL16 (fromIntegral r)
        ag = amultTL16 (fromIntegral g) `unsafeShiftR` 8
        ab = amultTL16 (fromIntegral b) `unsafeShiftR` 16

{-# SPECIALISE INLINE crgbap :: Int -> Int -> Int -> Int -> CairoColor #-}
{-# SPECIALISE INLINE crgbap :: Word8 -> Word8 -> Word8 -> Word8 -> CairoColor #-}
-- | Construct a 'CairoColor' from already premultiplied red, green, blue and alpha between [0 .. min alpha 255].
-- Premultiplied colors means that translucent red is @crgbap 127 0 0 127@.
crgbap :: Integral a => a -> a -> a -> a -> CairoColor
crgbap !r !g !b !a = w a 24 .|. w r 16 .|. w g 8 .|. w b 0
  where w x s = fromIntegral x `unsafeShiftL` s

{-# INLINE cairoRaster #-}
-- | Create an image "from scratch" by specifying the pixel data.
cairoRaster :: (Int -> Int -> CairoColor) -> Int -> Int -> IO (Diagram Cairo R2)
cairoRaster !f !w !h = do
  (s,arr,_,coord) <- cairoBitmapArray w h
  sequence_ [ unsafeWrite arr (coord x y) (f x y) | y <- [0..(h-1)], x <- [0..(w-1)]]
  fmap image $ cairoSurfaceImage s w h

-- | Load an image for Cairo (JPG, PNG, BMP, TIFF ...). The image is loaded once and
-- which can be useful for e.g. tiles.
cairoLoadImage :: FilePath -> IO (Diagram Cairo R2)
cairoLoadImage fp = dynamicImageToCairo =<< either fail return =<< JC.readImage fp

-- | Convert a JuicyPixel DynamicImage into a Diagrams Cairo image.
dynamicImageToCairo :: JC.DynamicImage -> IO (Diagram Cairo R2)
dynamicImageToCairo di =
  case di of
    (JC.ImageRGBA8 i)  -> cairoRaster (rgbaConv i)  (JC.imageWidth i) (JC.imageHeight i)
    (JC.ImageRGB8 i)   -> cairoRaster (rgbConv i)   (JC.imageWidth i) (JC.imageHeight i)
    (JC.ImageYCbCr8 i) -> cairoRaster (ycbcrConv i) (JC.imageWidth i) (JC.imageHeight i)
    _                  -> fail "Unsupported DynamicImage format in dynamicImageToCairo"

{-# INLINE cairoBitmapArray #-}
-- | Create a new surface and array for drawing image data and feeding into 'cairoSurfaceImage'.
-- Takes bitmap width and height as parameters. The return values are:
-- 'Surface' is for feeding 'cairoSurfaceImage'.
-- 'SurfaceData Int CairoColor' is an MArray instance referencing the image data in the surface.
-- The image is in the format Cairo expects, which means there may be gaps inside it.
-- The third result is 'Stride' expressed in 32-bit words.
-- The last
-- result is a function mapping 'x' and 'y' coordinates to coordinates in the array.
cairoBitmapArray :: Int
                 -> Int
                 -> IO (C.Surface, C.SurfaceData Int CairoColor,Int,Int->Int->Int)
cairoBitmapArray w h = do
  s <- C.createImageSurface C.FormatARGB32 w h
  arr <- C.imageSurfaceGetPixels s :: IO (C.SurfaceData Int CairoColor)
  rowstridew <- fmap (`div` 4) $ C.imageSurfaceGetStride s
  let coord :: Int -> Int -> Int
      coord x y = y * rowstridew + x
  return (s, arr, rowstridew, coord)

-- | Create an Diagrams image from a Cairo image surface and from width and height in pixels.
cairoSurfaceImage :: C.Surface -> Int -> Int -> IO (DImage (Native CairoSurface))
cairoSurfaceImage s w h = do
  C.surfaceMarkDirty s
  return (DImage (ImageNative (CairoSurface s)) w h mempty)

{-# INLINE rgbConv #-}
rgbConv :: JC.Image JC.PixelRGB8 -> Int -> Int -> CairoColor
rgbConv img x y = f $ JC.unsafePixelAt (JC.imageData img) idx
  where f (JC.PixelRGB8 r g b) = crgb r g b
        idx = (x + y * JC.imageWidth img) * 3

{-# INLINE rgbaConv #-}
rgbaConv :: JC.Image JC.PixelRGBA8 -> Int -> Int -> CairoColor
rgbaConv img x y = f $ JC.unsafePixelAt (JC.imageData img) idx
  where f (JC.PixelRGBA8 r g b a) = crgba r g b a
        idx = (x + y * JC.imageWidth img) * 4

{-# INLINE ycbcrConv #-}
ycbcrConv :: JC.Image JC.PixelYCbCr8 -> Int -> Int -> CairoColor
ycbcrConv img x y = f $ JC.unsafePixelAt (JC.imageData img) idx
  where fr (JC.PixelRGB8 r g b) = crgb r g b
        f :: JC.PixelYCbCr8 -> CairoColor
        f = fr . JC.convertPixel
        idx = (x + y * JC.imageWidth img) * 3

-- | Wrap Cairo surfaces for native images so they are Typeable.
data CairoSurface = CairoSurface !C.Surface deriving Typeable

instance Renderable (DImage (Native CairoSurface)) Cairo where
  render _ (DImage (ImageNative (CairoSurface isurf)) w h tr) = C . liftC $ do
    C.save
    cairoTransf (tr <> reflectionY)
    C.setSourceSurface isurf (-fromIntegral w / 2) (-fromIntegral h / 2)
    C.paint
    C.restore

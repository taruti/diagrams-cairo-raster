{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-|
Module      : Diagrams.Backend.Cairo.Raster
Description : Cairo specific native raster images (bitmaps) for Diagrams with repa support.
Copyright   : (c) Taru Karttunen, 2014
License     : BSD3
Maintainer  : taruti@taruti.net
Stability   : experimental

Cairo specific native images for Diagrams.
-}
module Diagrams.Backend.Cairo.Raster(
  -- * High-level API
  cairoRaster, cairoLoadImage,
  -- * Colors
  CairoColor,
  crgb, crgba, crgbap,
  -- * Repa API
  computeDImageP, computeIntoP,
  -- * Low-level API
  CairoSurface(..),
  dynamicImageToCairo,
  cairoBitmapArray,
  cairoSurfaceImage
  ) where

import qualified Codec.Picture as JC
import qualified Codec.Picture.Types as JC
import Data.Array.Base(MArray(unsafeWrite))
import Data.Array.Repa as R
import Data.Array.Repa.Eval
import Data.Bits(Bits(..))
import Data.Typeable
import Data.Word
import qualified Graphics.Rendering.Cairo as C
import Diagrams.Backend.Cairo.Internal
import Diagrams.Prelude

-- | An ARGB color as understood by Cairo. Cairo colors are with pre-multiplied alpha in a native endian format with the bytes from low to high: blue, green, red and alpha.
type CairoColor = Word32

{-# SPECIALISE INLINE crgb :: Int -> Int -> Int -> CairoColor #-}
{-# SPECIALISE INLINE crgb :: Word8 -> Word8 -> Word8 -> CairoColor #-}
-- | Construct a 'CairoColor' from red, green and blue between [0 .. 255].
crgb :: Integral a => a -> a -> a -> CairoColor
crgb r g b = 0xFF000000 .|. w r 16 .|. w g 8 .|. w b 0
  where w x s = fromIntegral x `unsafeShiftL` s

{-# SPECIALISE INLINE crgba :: Int -> Int -> Int -> Int -> CairoColor #-}
{-# SPECIALISE INLINE crgba :: Word8 -> Word8 -> Word8 -> Word8 -> CairoColor #-}
-- | Construct a 'CairoColor' from non-premultiplied red, green, blue and alpha between [0 .. 255].
-- Non-premultiplied colors means that translucent red is @crgba 255 0 0 127@.
crgba :: Integral a => a -> a -> a -> a -> CairoColor
crgba r g b a = fromIntegral ((fromIntegral a `unsafeShiftL` 24) .|. ar .|. ag .|. ab)
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
crgbap r g b a = w a 24 .|. w r 16 .|. w g 8 .|. w b 0
  where w x s = fromIntegral x `unsafeShiftL` s

{-# INLINE cairoRaster #-}
-- | Create an image "from scratch" by specifying the pixel data.
cairoRaster :: (Int -> Int -> CairoColor) -> Int -> Int -> IO (DImage (Native CairoSurface))
cairoRaster f w h = do
  (s,arr,_,coord) <- cairoBitmapArray w h
  sequence_ [ unsafeWrite arr (coord x y) (f x y) | y <- [0..(h-1)], x <- [0..(w-1)]] 
  cairoSurfaceImage s w h

-- | Load an image for Cairo (JPG, PNG, BMP, TIFF ...). The image is loaded once and 
-- which can be useful for e.g. tiles.
cairoLoadImage :: FilePath -> IO (DImage (Native CairoSurface))
cairoLoadImage fp = dynamicImageToCairo =<< either fail return =<< JC.readImage fp

-- | Convert a JuicyPixel DynamicImage into a Diagrams Cairo image.
dynamicImageToCairo :: JC.DynamicImage -> IO (DImage (Native CairoSurface))
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

{-# INLINE computeDImageP #-}
-- | Compute a DImage from a 'repa' array with the supplied width and height. 
-- The function calculating the array receives a 'DIM2' containing the dimensions
-- of the array which may be /different/ from the image width and height specified
-- due to Cairo requirements of image data alignment. The 'DIM2' is in the form
-- @(Z :. height :. width)@.
computeDImageP :: Load r1 sh CairoColor => Int -> Int -> (DIM2 -> Array r1 sh CairoColor) -> IO (DImage (Native CairoSurface))
computeDImageP !w0 !h0 !arr = do
  (s,sd,w',_) <- cairoBitmapArray w0 h0
  computeIntoP sd $ arr (Z :. h0 :. w')
  cairoSurfaceImage s w0 h0

{-# INLINE computeIntoP #-}
-- | Compute a 'repa' array into a 'SurfaceData'.
computeIntoP :: Load r1 sh CairoColor => C.SurfaceData Int CairoColor -> Array r1 sh CairoColor -> IO ()
computeIntoP !sd !arr = loadP arr (CF sd)

data CF
instance Target CF CairoColor where
 data MVec CF CairoColor = CF (C.SurfaceData Int CairoColor)
 newMVec n = error "CF newMVec NIMP"
 {-# INLINE newMVec #-}
 unsafeWriteMVec !(CF cf) !ix !x = unsafeWrite cf ix x
 {-# INLINE unsafeWriteMVec #-}
 unsafeFreezeMVec !sh _ = error "CF unsafeFreezeMVec NIMP"
 {-# INLINE unsafeFreezeMVec #-}
 deepSeqMVec !(CF fp) x = fp `seq` x
 {-# INLINE deepSeqMVec #-}
 touchMVec !(CF fp) = fp `seq` return ()
 {-# INLINE touchMVec #-}

-- | Wrap Cairo surfaces for native images so they are Typeable.
data CairoSurface = CairoSurface !C.Surface deriving Typeable

instance Renderable (DImage (Native CairoSurface)) Cairo where
  render _ (DImage (ImageNative (CairoSurface isurf)) w h tr) = C . liftC $ do
    C.save
    cairoTransf (tr <> reflectionY)
    C.setSourceSurface isurf (-fromIntegral w / 2) (-fromIntegral h / 2)
    C.paint
    C.restore

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-|
Module      : Diagrams.Backend.Cairo.Raster.Repa
Description : Draw 'repa' arrays into Cairo diagrams.
Copyright   : (c) Taru Karttunen, 2014
License     : BSD3
Maintainer  : taruti@taruti.net
Stability   : experimental

Draw 'repa' arrays into Cairo diagrams.
-}
module Diagrams.Backend.Cairo.Raster.Repa(
  cairoRepa, computeIntoP, computeIntoS
  ) where

import           Data.Array.Base                 (MArray (unsafeWrite))
import           Data.Array.Repa                 as R
import           Data.Array.Repa.Eval
import           Diagrams.Backend.Cairo.Internal
import           Diagrams.Backend.Cairo.Raster.Internal
import           Diagrams.Prelude
import qualified Graphics.Rendering.Cairo        as C


{-# INLINE cairoRepa #-}
-- | Compute a DImage from a 'repa' array with the supplied width and height.
-- The function calculating the array receives a 'DIM2' containing the dimensions
-- of the array which may be /different/ from the image width and height specified
-- due to Cairo requirements of image data alignment. The 'DIM2' is in the form
-- @(Z :. height :. width)@.
cairoRepa :: Load r1 sh CairoColor => (DIM2 -> Array r1 sh CairoColor) -> Int -> Int -> IO (Diagram Cairo R2)
cairoRepa !afun !w0 !h0 = do
  (s,sd,w',_) <- cairoBitmapArray w0 h0
  computeIntoP sd $ afun (Z :. h0 :. w')
  fmap image $ cairoSurfaceImage s w0 h0

{-# INLINE computeIntoP #-}
-- | Low-level primitive: Compute a 'repa' array into a 'SurfaceData' in parallel.
computeIntoP :: Load r1 sh CairoColor => C.SurfaceData Int CairoColor -> Array r1 sh CairoColor -> IO ()
computeIntoP !sd !arr = loadP arr (CF sd)

{-# INLINE computeIntoS #-}
-- | Low-level primitive: Compute a 'repa' array into a 'SurfaceData' sequentally.
computeIntoS :: Load r1 sh CairoColor => C.SurfaceData Int CairoColor -> Array r1 sh CairoColor -> IO ()
computeIntoS !sd !arr = loadS arr (CF sd)

data CF
instance Target CF CairoColor where
 data MVec CF CairoColor = CF (C.SurfaceData Int CairoColor)
 newMVec _n = error "CF newMVec NIMP"
 {-# INLINE newMVec #-}
 unsafeWriteMVec !(CF cf) !ix !x = unsafeWrite cf ix x
 {-# INLINE unsafeWriteMVec #-}
 unsafeFreezeMVec !_sh _ = error "CF unsafeFreezeMVec NIMP"
 {-# INLINE unsafeFreezeMVec #-}
 deepSeqMVec !(CF fp) x = fp `seq` x
 {-# INLINE deepSeqMVec #-}
 touchMVec !(CF fp) = fp `seq` return ()
 {-# INLINE touchMVec #-}


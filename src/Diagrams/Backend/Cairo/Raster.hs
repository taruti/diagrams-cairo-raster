{-|
Module      : Diagrams.Backend.Cairo.Raster
Description : Draw raster/bitmap images for the Diagrams Cairo backend.
Copyright   : (c) Taru Karttunen, 2014
License     : BSD3
Maintainer  : taruti@taruti.net
Stability   : experimental

Draw raster/bitmap images for the Diagrams Cairo backend.
-}
module Diagrams.Backend.Cairo.Raster(
  -- * High-level API
  cairoRaster, cairoLoadImage,
  -- * Colors
  CairoColor,
  crgb, crgba, crgbap
  ) where

import Diagrams.Backend.Cairo.Raster.Internal

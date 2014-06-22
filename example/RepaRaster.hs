import Data.Array.Repa
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Raster
import Diagrams.Backend.Cairo.Raster.Repa
import Diagrams.Prelude

dia :: Int -> Int -> IO (Diagram Cairo R2)
dia = cairoRepa (\d -> fromFunction d rplus)

rplus :: DIM2 -> CairoColor
rplus (Z :. y :. x) = crgb x y (x*y)

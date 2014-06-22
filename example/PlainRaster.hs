import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Raster
import Diagrams.Prelude

dia :: Int -> Int -> IO (Diagram Cairo R2)
dia = cairoRaster cplus

cplus :: Int -> Int -> CairoColor
cplus x y = crgb x y (x*y)

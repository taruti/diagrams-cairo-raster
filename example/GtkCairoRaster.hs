import GtkHelper
import Diagrams.Backend.Cairo.Raster

main :: IO ()
main = gtkDiaMain =<< cairoRaster fun 200 100

fun :: Int -> Int -> CairoColor
fun x y = let c = round (255 * sin (fromIntegral x :: Double) * cos (fromIntegral y))
          in if c >= (0 :: Int) then crgb 255 c 255 else crgb 255 255 (-c)

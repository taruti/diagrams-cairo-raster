import GtkHelper
import Data.Array.Repa
import Diagrams.Backend.Cairo.Raster
import Diagrams.Backend.Cairo.Raster.Repa

main :: IO ()
main = gtkDiaMain =<< cairoRepa (\d -> fromFunction d fun) 200 100

fun :: DIM2 -> CairoColor
fun (Z :. y :. x) = let c = round (255 * sin (fromIntegral x :: Double) * cos (fromIntegral y))
                    in if c >= (0 :: Int) then crgb 255 c 255 else crgb 255 255 (-c)

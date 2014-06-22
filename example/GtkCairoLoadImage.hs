import GtkHelper
import Diagrams.Backend.Cairo.Raster
import Diagrams.Prelude

main :: IO ()
main = do
  dia <- cairoLoadImage "Haskell-Logo-Variation.png"
  gtkDiaMain $ mconcat $ take 30 [ rotate (x @@ deg) dia | x <- fibs ]

fibs :: [Double]
fibs = 1:2:zipWith (+) fibs (tail fibs)

module GtkHelper(gtkDiaMain) where

import Graphics.UI.Gtk
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Diagrams.Backend.Cairo.Internal
import Diagrams.Prelude

gtkDiaMain :: Diagram Cairo R2 -> IO ()
gtkDiaMain dia = do
  void $ initGUI
  window <- windowNew
  void $ onDestroy window mainQuit
  set window [ containerBorderWidth := 10, windowTitle := "Diagrams.Backend.Cairo.Raster" ]
  da  <- drawingAreaNew
  void $ da `on` exposeEvent $ liftIO $ do
    dw <- widgetGetDrawWindow da
    let (_,r) = renderDia Cairo (CairoOptions "" (Width 300) PNG False) dia
    renderWithDrawable dw r
    return True
  set window [ containerChild := da ]
  widgetShowAll window
  mainGUI


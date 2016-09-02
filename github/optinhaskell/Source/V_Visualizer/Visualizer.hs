module Main where

import System.Time
import System.Cmd
import System.Process
import Text.Printf
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Control.Concurrent
import Data.IORef
import Random
import Text.Printf
import List

-- :: --

main = do
	initGUI
	Just xml <- xmlNew "Visualizer.glade"
	
	window   <- xmlGetWidget xml castToWindow "window"
	onDestroy window mainQuit
	
	acoim <- xmlGetWidget xml castToImage "aco_image"
	gaim <- xmlGetWidget xml castToImage "ga_image"
	saim <- xmlGetWidget xml castToImage "sa_image"
	
	{-
	Right acoPic <- pixbufNewFromFile "ant.png"
	Right saPic <- pixbufNewFromFile "sa.png"
	Right gaPic <- pixbufNewFromFile "ga.png"
		
	imageSetFromPixbuf  acoim acoPic
	imageSetFromPixbuf  saim saPic
	imageSetFromPixbuf  gaim gaPic
	-}
	imageSetFromFile acoim "ant.png"
	imageSetFromFile gaim "ga.png"
	imageSetFromFile saim "sa.png"
	aco <- xmlGetWidget xml castToButton "aco_button"
	ga <- xmlGetWidget xml castToButton "ga_button"
	sa <- xmlGetWidget xml castToButton "sa_button"
	about <- xmlGetWidget xml castToButton "about_button"
		
	aco `onClicked` do
					runInteractiveProcess "ACO.exe" [] Nothing Nothing
					return ()
	
	ga `onClicked` do
					runInteractiveProcess "GA.exe" [] Nothing Nothing
					return ()
	sa `onClicked` do
					runInteractiveProcess "SA.exe" [] Nothing Nothing
					return ()
					
	about `onClicked` do 
					showAboutDialog window
	widgetShowAll window
	mainGUI
	
	
showAboutDialog :: Window -> IO ()
showAboutDialog parent = do
  -- create the about dialog
  aboutDialog <- aboutDialogNew
  
  -- set some attributes
  Right logo <- pixbufNewFromFile "about.png"
  aboutDialogSetLogo aboutDialog (Just logo)
  set aboutDialog [
      aboutDialogName      := "Visualizer Info",
      aboutDialogVersion   := "0.1",
      aboutDialogCopyright := "Pham Thanh Lam",
      aboutDialogComments  := "Heuristic search in AI",
      aboutDialogWebsite   := "http://groups.google.com/group/vnhaskell",
      aboutDialogAuthors   := ["lam.thanh.pham@gmail.com"]
    ]
  
  -- make the about dialog appear above the main window
  windowSetTransientFor aboutDialog parent
  
  -- make the dialog non-modal. When the user closes the dialog destroy it.
  aboutDialog `afterResponse` (\_ -> widgetDestroy aboutDialog)
  widgetShow aboutDialog

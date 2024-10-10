module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Controller.Controller
import Model.Model
import View.View

window :: Display
window = InWindow "Hac-Man" (1280, 720) (0, 0)
-- window = FullScreen

main :: IO ()
main = do
        playIO 
          window 
          black 
          30 
          initialState 
          render 
          eventHandler 
          step

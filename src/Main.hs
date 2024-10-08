module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Controller
import Model
import View


main :: IO ()
main = do
        playIO 
          FullScreen 
          black 
          30 
          initialState 
          render 
          eventHandler 
          step

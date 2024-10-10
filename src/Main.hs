module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Controller.Controller
import Model.Model
import View.View

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

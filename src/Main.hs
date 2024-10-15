module Main where

import Controller.Controller
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model.Model
import View.View

-- needed when opening in windowed mode
-- import View.Transform

window :: Display
-- window = InWindow "Hac-Man" gameArea (0, 0)
window = FullScreen

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

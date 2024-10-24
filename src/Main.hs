module Main where

import Controller.Controller
import Controller.EntityController
import Model.Entities

import qualified Data.Map                         as Map

import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.IO.Game as GlossIO

import           Model.CustomMaze
import           Model.Model

import           View.RenderMaze
import           View.View

-- needed when opening in windowed mode
-- import View.Transform

window :: Gloss.Display
-- window = InWindow "Hac-Man" gameArea (0, 0)
window = Gloss.FullScreen

initialState :: Map.Map Name Sprite -> GameState
initialState x =
  MkGameState
    { status = Running
    , maze = customMaze
    , sprites = x
    , elapsedTime = 0
    , enableDebug = True
    , windowInfo = MkWindowInfo (0, 0)
    , player = initiatePlayer
    , ghosts = [initiateblinky]
    }

main :: IO ()
main = do
  -- load sprites into a map of sprites
  sp <- loadActiveSprites
  let spriteMap = storeActiveSprites sp

  GlossIO.playIO
    window
    Gloss.black
    60
    (initialState spriteMap)
    render
    eventHandler
    step

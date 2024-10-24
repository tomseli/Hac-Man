module Main where

import           Controller.Controller

import qualified Data.Map                         as Map

import qualified Graphics.Gloss                   as Gloss
import qualified Graphics.Gloss.Interface.IO.Game as GlossIO

import           Model.CustomMaze
import           Model.Entities
import           Model.Model

import           View.EntityAnimation
import           View.RenderMaze
import           View.View

-- needed when opening in windowed mode
-- import View.Transform

window :: Gloss.Display
-- window = InWindow "Hac-Man" gameArea (0, 0)
window = Gloss.FullScreen

initialState :: GameState
initialState =
  MkGameState
    { status = Running
    , maze = customMaze
    , sprites = Map.empty
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
  pAnim <- loadPlayerAnimation

  -- store the new info in state
  let state = (storeActiveSprites sp . storePlayerAnimation pAnim) initialState

  GlossIO.playIO
    window
    Gloss.black
    60
    state
    render
    eventHandler
    step

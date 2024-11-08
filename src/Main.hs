module Main where

import           Controller.Controller
import           Controller.EntityController

import qualified Data.Map                         as Map

import qualified Graphics.Gloss                   as Gloss
import qualified Graphics.Gloss.Interface.IO.Game as GlossIO

import           Model.CustomMaze
import           Model.Entities
import           Model.Model

import           View.EntityAnimation
import           View.RenderMaze
import           View.SaveHighScore
import           View.Transform
import           View.View


window :: Gloss.Display
window = GlossIO.InWindow "Hac-Man" gameArea (0, 0)
-- window = Gloss.FullScreen

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
    , pelletC = cntPellets customMaze
    , unfrightenTime = 0
    , highscores = []  -- not yet loaded
    }

main :: IO ()
main = do
  -- load sprites into a map of sprites
  sp <- loadActiveSprites
  playerAnimation <- loadPlayerAnimation
  blinkyAnimation <- loadBlinkyAnimation
  highScoreContents  <- readFile "src/highscores.txt"

  -- store the new info in state
  let
    state =
        ( storeActiveSprites sp
        . storePlayerAnimation playerAnimation
        . storeGhostAnimation [blinkyAnimation]
        . loadHighScores highScoreContents
        ) initialState

  GlossIO.playIO
    window
    Gloss.black
    60
    state
    render
    eventHandler
    step

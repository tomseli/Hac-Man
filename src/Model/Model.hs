module Model.Model where

import Controller.EntityController
import Model.CustomMaze
import Model.Entities
import Model.Maze

data GameStatus = Running | GameOver | Paused | Quitting deriving Eq

newtype WindowInfo = MkWindowInfo {resolution :: (Int, Int)}

data GameState = MkGameState
  { status :: GameStatus
  , maze :: Maze
  , elapsedTime :: Float
  , enableDebug :: Bool
  , windowInfo :: WindowInfo
  , player :: Player
  }


initialState :: GameState
initialState =
  MkGameState
    { status = Running
    , maze = customMaze
    , elapsedTime = 0
    , enableDebug = True
    , windowInfo = MkWindowInfo (0, 0)
    , player = testPlayer
    }
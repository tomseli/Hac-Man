module Model.Model where

import Controller.EntityController
import Model.Entities
import Model.Maze (Maze, MazeShape, buildTestMaze)

data GameStatus = Running | GameOver | Paused | Quitting

newtype WindowInfo = MkWindowInfo {resolution :: (Int, Int)}

data GameState = MkGameState
  { status :: GameStatus
  , mazeShape :: MazeShape
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
    , mazeShape = (32, 32) -- match with given maze
    , maze = buildTestMaze 32
    , elapsedTime = 0
    , enableDebug = True
    , windowInfo = MkWindowInfo (0, 0)
    , player = testPlayer
    }
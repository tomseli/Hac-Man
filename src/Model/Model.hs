module Model.Model where

import Controller.EntityController ( testPlayer )
import Model.Entities ( Player )
import Model.Maze (Maze, MazeShape, buildTestMaze)
import Model.CustomMaze

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
    , maze = customMaze
    , elapsedTime = 0
    , enableDebug = True
    , windowInfo = MkWindowInfo (0, 0)
    , player = testPlayer
    }
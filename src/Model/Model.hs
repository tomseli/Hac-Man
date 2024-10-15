module Model.Model where 

import Model.Maze ( Maze, MazeShape, buildTestMaze )

import qualified Graphics.Gloss.Data.Picture as Gloss

import qualified Data.Map as Map 
import Model.Entities  
import Controller.EntityController 

data GameStatus = Running | GameOver | Paused | Quitting
newtype WindowInfo = MkWindowInfo {resolution :: (Int, Int)}

data GameState = MkGameState 
                  { status          :: GameStatus
                  , mazeShape       :: MazeShape
                  , maze            :: Maze
                  , elapsedTime     :: Float
                  , enableDebug     :: Bool
                  , windowInfo      :: WindowInfo
                  , player          :: Player
                  , player          :: Player
                  }

initialState :: GameState
initialState = MkGameState 
                { status      = Running 
                , mazeShape   = (32, 32) -- match with given maze
                , maze        = buildTestMaze 32
                , elapsedTime = 0 
                , enableDebug = True
                , windowInfo  = MkWindowInfo (0, 0)
                , player      = testPlayer
                }
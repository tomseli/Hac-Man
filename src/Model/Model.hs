module Model.Model where

import Graphics.Gloss as Gloss
import Model.Entities
import Model.Maze

data GameStatus = Running | GameOver | Paused | Quitting

newtype WindowInfo = MkWindowInfo {resolution :: (Int, Int)}

data GameState = MkGameState
  { status :: GameStatus
  , maze :: Maze
  , mazePicture :: Gloss.Picture
  , elapsedTime :: Float
  , enableDebug :: Bool
  , windowInfo :: WindowInfo
  , player :: Player
  , ghosts :: [Ghost]
  }
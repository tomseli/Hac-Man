module Model.Model where

import qualified Data.Map as Map
import Graphics.Gloss as Gloss
import Model.Entities
import Model.Maze

type Name = String

type Sprite = Gloss.Picture

type Sprites = Map.Map Name Sprite

data GameStatus = Running | GameOver | Paused | Quitting

newtype WindowInfo = MkWindowInfo {resolution :: (Int, Int)}

data GameState = MkGameState
  { status :: GameStatus
  , maze :: Maze
  , sprites :: Sprites
  , elapsedTime :: Float
  , enableDebug :: Bool
  , windowInfo :: WindowInfo
  , player :: Player
  }
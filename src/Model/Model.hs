module Model.Model where

import qualified Data.Map       as Map

import           Graphics.Gloss as Gloss

import           Model.Entities
import           Model.Maze

type Name = String

type Sprite = Gloss.Picture

type Sprites = Map.Map Name Sprite

data GameStatus = Running | GameOver | Paused | Quitting deriving (Eq, Show)

newtype WindowInfo = MkWindowInfo {resolution :: (Int, Int)}

type HighScores = [String]

data GameState = MkGameState
  { status         :: GameStatus
  , maze           :: Maze
  , sprites        :: Sprites
  , elapsedTime    :: Float
  , enableDebug    :: Bool
  , windowInfo     :: WindowInfo
  , player         :: Player
  , ghosts         :: [Ghost]
  , pelletC        :: (Int, Int) -- total vs eaten
  , unfrightenTime :: Float
  , highscores     :: HighScores
  , level          :: Int
  }

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
  { status      :: GameStatus
  , maze        :: Maze
  , isNewMaze   :: Bool --boolean flag if the Maze has changed or not
  , oldMaze     :: Gloss.Picture
  , sprites     :: Sprites --map of sprites
  , elapsedTime :: Float
  , deltaTime   :: Float
  , enableDebug :: Bool
  , windowInfo  :: WindowInfo --resolution of window
  , player      :: Player
  , ghosts      :: [Ghost]
  , pelletC     :: (Int, Int) -- total vs eaten
  , highscores  :: HighScores -- list of highscores (retrieved from file)
  , level       :: Int        -- current level
  }

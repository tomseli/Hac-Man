module Model.Maze where

import qualified Data.Map as Map
import GHC.Conc.Sync (newTVar)

data CornerOrientation = SE | NW | NE | SW deriving (Show)

data WallOrientation = Horizontal | Vertical deriving (Show)

data WallShape
  = MkCorner CornerOrientation
  | MkWallShape WallOrientation
  deriving (Show)

data ConsumableType = Pellet | SuperPellet | Cherry deriving (Show)

data FloorType = MkConsumable ConsumableType | EmptyTile deriving (Show)

data Tile = MkFloor FloorType | MkWall WallShape deriving (Show)

type TilePosition = (Int, Int)

type Maze = Map.Map TilePosition Tile

-- Constructs a text maze with x*x size
-- does this function have redundant guards? yes
-- am i going to make a function purely for testing pretty? no
-- the maze should have the shape listed below
{-
XXXX
X  X
X  X
XXXX
-}
testMaze :: Int -> Maze
testMaze n = go 0 Map.empty
  where
    go x m
      | x >= n * n          = m -- base case
      | snd coord == 0     = go (x + 1) (Map.insert coord wallH m) -- top row
      | snd coord == n - 1 = go (x + 1) (Map.insert coord wallH m) -- bottom row
      | fst coord == 0     = go (x + 1) (Map.insert coord wallV m) -- left column
      | fst coord == n - 1 = go (x + 1) (Map.insert coord wallV m) -- right column
      | otherwise          = go (x + 1) (Map.insert coord empty m) -- everything else
      where
        coord = (x `mod` n, x `div` n)
        wallH = MkWall (MkWallShape Horizontal)
        wallV = MkWall (MkWallShape Vertical)
        empty = MkFloor EmptyTile


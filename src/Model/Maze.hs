module Model.Maze where

import qualified Data.Map as Map

data CornerOrientation = SE | NW | NE | SW deriving (Show)

data WallOrientation = Horizontal | Vertical deriving (Show)

data WallShape
  = MkCorner CornerOrientation
  | MkWallShape WallOrientation
  deriving (Show)

data ConsumableType = Pellet | SuperPellet deriving (Show, Eq)

data FloorType = MkConsumable ConsumableType | EmptyTile deriving (Show)

data Tile = MkFloor FloorType | MkWall WallShape deriving (Show)

type TilePosition = (Int, Int)

type Maze = Map.Map TilePosition Tile

-- Constructs a text maze with x*x size
-- BUG: The ordering of the printing might not be correct!
buildTestMaze :: Int -> Maze
buildTestMaze n = go 0 Map.empty
 where
  go x m
    | x >= n * n = m -- base case
    | fst coord == 0 = go (x + 1) (Map.insert coord wallV m)
    | fst coord == n - 1 = go (x + 1) (Map.insert coord wallV m)
    | snd coord == 0 = go (x + 1) (Map.insert coord wallH m)
    | snd coord == n - 1 = go (x + 1) (Map.insert coord wallH m)
    | otherwise = go (x + 1) (Map.insert coord empty m)
   where
    coord = (x `mod` n, x `div` n)
    wallH = MkWall (MkWallShape Horizontal)
    wallV = MkWall (MkWallShape Vertical)
    empty = MkFloor EmptyTile

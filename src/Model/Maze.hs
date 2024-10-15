module Model.Maze where

import qualified Data.Map as Map

data CornerOrientation = SE | NW | NE | SW
data WallOrientation = Horizontal | Vertical

data WallShape = MkCorner CornerOrientation
               | MkWallShape WallOrientation

data ConsumableType = Pellet | SuperPellet | Cherry

data FloorType = MkConsumable ConsumableType | EmptyTile
data Tile = MkFloor FloorType | MkWall WallShape

type TilePosition = (Int , Int)
type Maze = Map.Map TilePosition Tile 

{-
XXXX
X  X
X  X
XXXX
-}

-- Constructs a text maze with x*x size
-- does this function have redundant guards? yes
-- am i going to make a function purely for testing pretty? no
testMaze :: Int -> Maze
testMaze size = go 0 Map.empty
  where go x m | x >= (size - 1)             = m         -- base case
               | snd coord == 0              = go (x+1) (Map.insert coord wallH m) -- top row
               | snd coord == (^!) size - 1  = go (x+1) (Map.insert coord wallH m) -- bottom row
               | fst coord == 0              = go (x+1) (Map.insert coord wallV m) -- left column
               | fst coord == (^!) size - 1  = go (x+1) (Map.insert coord wallV m) -- right column
               | otherwise                   = go (x+1) (Map.insert coord empty m) -- everything else
               where 
                  coord = (x `mod` (^!) size, size `div` (^!) size)
                  wallH = MkWall (MkWallShape Horizontal)
                  wallV = MkWall (MkWallShape Vertical)
                  empty = MkFloor EmptyTile

-- yoinked from
-- https://wiki.haskell.org/Generic_number_type#squareRoot
(^!) :: Int -> Int
(^!) n = try 0 
  where try i | i*i <= n    = try (i + 1) 
              | otherwise   = i - 1
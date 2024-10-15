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
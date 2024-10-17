module Out where

import qualified Data.Map as Map
               

customMaze :: Maze
customMaze = foldr f Map.empty xs
 where 
  f (k, v) = Map.insert k v
  xs = [((0,0), MkWall (MkWallShape Vertical)),((1,0), MkWall (MkWallShape Vertical)),((2,0), MkWall (MkWallShape Vertical)),((3,0), MkWall (MkWallShape Vertical)),((4,0), MkWall (MkWallShape Vertical)),((5,0), MkWall (MkWallShape Vertical)),((6,0), MkWall (MkWallShape Vertical)),((0,1), MkWall (MkWallShape Vertical)),((1,1), MkFloor EmptyTile),((2,1), MkFloor EmptyTile),((3,1), MkFloor EmptyTile),((4,1), MkFloor EmptyTile),((5,1), MkFloor EmptyTile),((6,1), MkWall (MkWallShape Vertical)),((0,2), MkWall (MkWallShape Vertical)),((1,2), MkFloor EmptyTile),((2,2), MkWall (MkWallShape Vertical)),((3,2), MkFloor EmptyTile),((4,2), MkFloor EmptyTile),((5,2), MkFloor EmptyTile),((6,2), MkWall (MkWallShape Vertical)),((0,3), MkWall (MkWallShape Vertical)),((1,3), MkWall (MkWallShape Vertical)),((2,3), MkWall (MkWallShape Vertical)),((3,3), MkFloor EmptyTile),((4,3), MkFloor EmptyTile),((5,3), MkWall (MkWallShape Vertical)),((6,3), MkWall (MkWallShape Vertical)),((0,4), MkWall (MkWallShape Vertical)),((1,4), MkFloor EmptyTile),((2,4), MkFloor EmptyTile),((3,4), MkFloor EmptyTile),((4,4), MkFloor EmptyTile),((5,4), MkFloor EmptyTile),((6,4), MkWall (MkWallShape Vertical)),((0,5), MkWall (MkWallShape Vertical)),((1,5), MkWall (MkWallShape Vertical)),((2,5), MkWall (MkWallShape Vertical)),((3,5), MkWall (MkWallShape Vertical)),((4,5), MkWall (MkWallShape Vertical)),((5,5), MkWall (MkWallShape Vertical)),((6,5), MkWall (MkWallShape Vertical))]

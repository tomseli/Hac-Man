module Model.CustomMaze where

import Model.Maze

import qualified Data.Map as Map
               
-- generated using:
-- python .\scripts\mazebuilder.py .\scripts\data\maze.txt -o .\src\Model\CustomMaze.hs -m Model.CustomMaze -i Model.Maze -v
customMaze :: Maze
customMaze = foldr f Map.empty xs
 where 
  f (k, v) = Map.insert k v
  xs = 
    [ ((0,0), MkWall (MkWallShape Vertical))
    , ((1,0), MkWall (MkWallShape Vertical))
    , ((2,0), MkWall (MkWallShape Vertical))
    , ((3,0), MkWall (MkWallShape Vertical))
    , ((4,0), MkWall (MkWallShape Vertical))
    , ((5,0), MkWall (MkWallShape Vertical))
    , ((6,0), MkWall (MkWallShape Vertical))
    , ((7,0), MkWall (MkWallShape Vertical))
    , ((8,0), MkWall (MkWallShape Vertical))
    , ((9,0), MkWall (MkWallShape Vertical))
    , ((10,0), MkWall (MkWallShape Vertical))
    , ((11,0), MkWall (MkWallShape Vertical))
    , ((12,0), MkWall (MkWallShape Vertical))
    , ((13,0), MkWall (MkWallShape Vertical))
    , ((14,0), MkWall (MkWallShape Vertical))
    , ((15,0), MkWall (MkWallShape Vertical))
    , ((16,0), MkWall (MkWallShape Vertical))
    , ((17,0), MkWall (MkWallShape Vertical))
    , ((18,0), MkWall (MkWallShape Vertical))
    , ((19,0), MkWall (MkWallShape Vertical))
    , ((20,0), MkWall (MkWallShape Vertical))
    , ((21,0), MkWall (MkWallShape Vertical))
    , ((22,0), MkWall (MkWallShape Vertical))
    , ((23,0), MkWall (MkWallShape Vertical))
    , ((24,0), MkWall (MkWallShape Vertical))
    , ((25,0), MkWall (MkWallShape Vertical))
    , ((26,0), MkWall (MkWallShape Vertical))
    , ((27,0), MkWall (MkWallShape Vertical))
    , ((0,1), MkWall (MkWallShape Vertical))
    , ((1,1), MkFloor EmptyTile)
    , ((2,1), MkFloor EmptyTile)
    , ((3,1), MkFloor EmptyTile)
    , ((4,1), MkFloor EmptyTile)
    , ((5,1), MkFloor EmptyTile)
    , ((6,1), MkFloor EmptyTile)
    , ((7,1), MkFloor EmptyTile)
    , ((8,1), MkFloor EmptyTile)
    , ((9,1), MkFloor EmptyTile)
    , ((10,1), MkFloor EmptyTile)
    , ((11,1), MkFloor EmptyTile)
    , ((12,1), MkFloor EmptyTile)
    , ((13,1), MkFloor EmptyTile)
    , ((14,1), MkFloor EmptyTile)
    , ((15,1), MkFloor EmptyTile)
    , ((16,1), MkFloor EmptyTile)
    , ((17,1), MkFloor EmptyTile)
    , ((18,1), MkFloor EmptyTile)
    , ((19,1), MkFloor EmptyTile)
    , ((20,1), MkFloor EmptyTile)
    , ((21,1), MkFloor EmptyTile)
    , ((22,1), MkFloor EmptyTile)
    , ((23,1), MkFloor EmptyTile)
    , ((24,1), MkFloor EmptyTile)
    , ((25,1), MkFloor EmptyTile)
    , ((26,1), MkFloor EmptyTile)
    , ((27,1), MkWall (MkWallShape Vertical))
    , ((0,2), MkWall (MkWallShape Vertical))
    , ((1,2), MkFloor EmptyTile)
    , ((2,2), MkWall (MkWallShape Vertical))
    , ((3,2), MkWall (MkWallShape Vertical))
    , ((4,2), MkWall (MkWallShape Vertical))
    , ((5,2), MkWall (MkWallShape Vertical))
    , ((6,2), MkWall (MkWallShape Vertical))
    , ((7,2), MkWall (MkWallShape Vertical))
    , ((8,2), MkWall (MkWallShape Vertical))
    , ((9,2), MkWall (MkWallShape Vertical))
    , ((10,2), MkWall (MkWallShape Vertical))
    , ((11,2), MkWall (MkWallShape Vertical))
    , ((12,2), MkWall (MkWallShape Vertical))
    , ((13,2), MkFloor EmptyTile)
    , ((14,2), MkWall (MkWallShape Vertical))
    , ((15,2), MkWall (MkWallShape Vertical))
    , ((16,2), MkWall (MkWallShape Vertical))
    , ((17,2), MkWall (MkWallShape Vertical))
    , ((18,2), MkWall (MkWallShape Vertical))
    , ((19,2), MkWall (MkWallShape Vertical))
    , ((20,2), MkWall (MkWallShape Vertical))
    , ((21,2), MkWall (MkWallShape Vertical))
    , ((22,2), MkWall (MkWallShape Vertical))
    , ((23,2), MkWall (MkWallShape Vertical))
    , ((24,2), MkWall (MkWallShape Vertical))
    , ((25,2), MkWall (MkWallShape Vertical))
    , ((26,2), MkFloor EmptyTile)
    , ((27,2), MkWall (MkWallShape Vertical))
    , ((0,3), MkWall (MkWallShape Vertical))
    , ((1,3), MkFloor EmptyTile)
    , ((2,3), MkWall (MkWallShape Vertical))
    , ((3,3), MkFloor EmptyTile)
    , ((4,3), MkFloor EmptyTile)
    , ((5,3), MkFloor EmptyTile)
    , ((6,3), MkFloor EmptyTile)
    , ((7,3), MkFloor EmptyTile)
    , ((8,3), MkFloor EmptyTile)
    , ((9,3), MkFloor EmptyTile)
    , ((10,3), MkFloor EmptyTile)
    , ((11,3), MkFloor EmptyTile)
    , ((12,3), MkFloor EmptyTile)
    , ((13,3), MkFloor EmptyTile)
    , ((14,3), MkFloor EmptyTile)
    , ((15,3), MkFloor EmptyTile)
    , ((16,3), MkFloor EmptyTile)
    , ((17,3), MkFloor EmptyTile)
    , ((18,3), MkFloor EmptyTile)
    , ((19,3), MkFloor EmptyTile)
    , ((20,3), MkFloor EmptyTile)
    , ((21,3), MkFloor EmptyTile)
    , ((22,3), MkFloor EmptyTile)
    , ((23,3), MkFloor EmptyTile)
    , ((24,3), MkFloor EmptyTile)
    , ((25,3), MkWall (MkWallShape Vertical))
    , ((26,3), MkFloor EmptyTile)
    , ((27,3), MkWall (MkWallShape Vertical))
    , ((0,4), MkWall (MkWallShape Vertical))
    , ((1,4), MkFloor EmptyTile)
    , ((2,4), MkWall (MkWallShape Vertical))
    , ((3,4), MkFloor EmptyTile)
    , ((4,4), MkWall (MkWallShape Vertical))
    , ((5,4), MkWall (MkWallShape Vertical))
    , ((6,4), MkWall (MkWallShape Vertical))
    , ((7,4), MkWall (MkWallShape Vertical))
    , ((8,4), MkWall (MkWallShape Vertical))
    , ((9,4), MkWall (MkWallShape Vertical))
    , ((10,4), MkWall (MkWallShape Vertical))
    , ((11,4), MkWall (MkWallShape Vertical))
    , ((12,4), MkWall (MkWallShape Vertical))
    , ((13,4), MkFloor EmptyTile)
    , ((14,4), MkWall (MkWallShape Vertical))
    , ((15,4), MkWall (MkWallShape Vertical))
    , ((16,4), MkWall (MkWallShape Vertical))
    , ((17,4), MkWall (MkWallShape Vertical))
    , ((18,4), MkWall (MkWallShape Vertical))
    , ((19,4), MkWall (MkWallShape Vertical))
    , ((20,4), MkWall (MkWallShape Vertical))
    , ((21,4), MkWall (MkWallShape Vertical))
    , ((22,4), MkWall (MkWallShape Vertical))
    , ((23,4), MkWall (MkWallShape Vertical))
    , ((24,4), MkFloor EmptyTile)
    , ((25,4), MkWall (MkWallShape Vertical))
    , ((26,4), MkFloor EmptyTile)
    , ((27,4), MkWall (MkWallShape Vertical))
    , ((0,5), MkWall (MkWallShape Vertical))
    , ((1,5), MkFloor EmptyTile)
    , ((2,5), MkWall (MkWallShape Vertical))
    , ((3,5), MkFloor EmptyTile)
    , ((4,5), MkWall (MkWallShape Vertical))
    , ((5,5), MkFloor EmptyTile)
    , ((6,5), MkFloor EmptyTile)
    , ((7,5), MkFloor EmptyTile)
    , ((8,5), MkFloor EmptyTile)
    , ((9,5), MkFloor EmptyTile)
    , ((10,5), MkFloor EmptyTile)
    , ((11,5), MkFloor EmptyTile)
    , ((12,5), MkFloor EmptyTile)
    , ((13,5), MkFloor EmptyTile)
    , ((14,5), MkFloor EmptyTile)
    , ((15,5), MkFloor EmptyTile)
    , ((16,5), MkFloor EmptyTile)
    , ((17,5), MkFloor EmptyTile)
    , ((18,5), MkFloor EmptyTile)
    , ((19,5), MkFloor EmptyTile)
    , ((20,5), MkFloor EmptyTile)
    , ((21,5), MkFloor EmptyTile)
    , ((22,5), MkFloor EmptyTile)
    , ((23,5), MkWall (MkWallShape Vertical))
    , ((24,5), MkFloor EmptyTile)
    , ((25,5), MkWall (MkWallShape Vertical))
    , ((26,5), MkFloor EmptyTile)
    , ((27,5), MkWall (MkWallShape Vertical))
    , ((0,6), MkWall (MkWallShape Vertical))
    , ((1,6), MkFloor EmptyTile)
    , ((2,6), MkWall (MkWallShape Vertical))
    , ((3,6), MkFloor EmptyTile)
    , ((4,6), MkWall (MkWallShape Vertical))
    , ((5,6), MkFloor EmptyTile)
    , ((6,6), MkWall (MkWallShape Vertical))
    , ((7,6), MkWall (MkWallShape Vertical))
    , ((8,6), MkWall (MkWallShape Vertical))
    , ((9,6), MkWall (MkWallShape Vertical))
    , ((10,6), MkWall (MkWallShape Vertical))
    , ((11,6), MkWall (MkWallShape Vertical))
    , ((12,6), MkWall (MkWallShape Vertical))
    , ((13,6), MkFloor EmptyTile)
    , ((14,6), MkWall (MkWallShape Vertical))
    , ((15,6), MkWall (MkWallShape Vertical))
    , ((16,6), MkWall (MkWallShape Vertical))
    , ((17,6), MkWall (MkWallShape Vertical))
    , ((18,6), MkWall (MkWallShape Vertical))
    , ((19,6), MkWall (MkWallShape Vertical))
    , ((20,6), MkWall (MkWallShape Vertical))
    , ((21,6), MkWall (MkWallShape Vertical))
    , ((22,6), MkFloor EmptyTile)
    , ((23,6), MkWall (MkWallShape Vertical))
    , ((24,6), MkFloor EmptyTile)
    , ((25,6), MkWall (MkWallShape Vertical))
    , ((26,6), MkFloor EmptyTile)
    , ((27,6), MkWall (MkWallShape Vertical))
    , ((0,7), MkWall (MkWallShape Vertical))
    , ((1,7), MkFloor EmptyTile)
    , ((2,7), MkWall (MkWallShape Vertical))
    , ((3,7), MkFloor EmptyTile)
    , ((4,7), MkWall (MkWallShape Vertical))
    , ((5,7), MkFloor EmptyTile)
    , ((6,7), MkWall (MkWallShape Vertical))
    , ((7,7), MkFloor EmptyTile)
    , ((8,7), MkFloor EmptyTile)
    , ((9,7), MkFloor EmptyTile)
    , ((10,7), MkFloor EmptyTile)
    , ((11,7), MkFloor EmptyTile)
    , ((12,7), MkFloor EmptyTile)
    , ((13,7), MkFloor EmptyTile)
    , ((14,7), MkFloor EmptyTile)
    , ((15,7), MkFloor EmptyTile)
    , ((16,7), MkFloor EmptyTile)
    , ((17,7), MkFloor EmptyTile)
    , ((18,7), MkFloor EmptyTile)
    , ((19,7), MkFloor EmptyTile)
    , ((20,7), MkFloor EmptyTile)
    , ((21,7), MkWall (MkWallShape Vertical))
    , ((22,7), MkFloor EmptyTile)
    , ((23,7), MkWall (MkWallShape Vertical))
    , ((24,7), MkFloor EmptyTile)
    , ((25,7), MkWall (MkWallShape Vertical))
    , ((26,7), MkFloor EmptyTile)
    , ((27,7), MkWall (MkWallShape Vertical))
    , ((0,8), MkWall (MkWallShape Vertical))
    , ((1,8), MkFloor EmptyTile)
    , ((2,8), MkWall (MkWallShape Vertical))
    , ((3,8), MkFloor EmptyTile)
    , ((4,8), MkWall (MkWallShape Vertical))
    , ((5,8), MkFloor EmptyTile)
    , ((6,8), MkWall (MkWallShape Vertical))
    , ((7,8), MkFloor EmptyTile)
    , ((8,8), MkFloor EmptyTile)
    , ((9,8), MkFloor EmptyTile)
    , ((10,8), MkFloor EmptyTile)
    , ((11,8), MkFloor EmptyTile)
    , ((12,8), MkFloor EmptyTile)
    , ((13,8), MkFloor EmptyTile)
    , ((14,8), MkFloor EmptyTile)
    , ((15,8), MkFloor EmptyTile)
    , ((16,8), MkFloor EmptyTile)
    , ((17,8), MkFloor EmptyTile)
    , ((18,8), MkFloor EmptyTile)
    , ((19,8), MkFloor EmptyTile)
    , ((20,8), MkFloor EmptyTile)
    , ((21,8), MkWall (MkWallShape Vertical))
    , ((22,8), MkFloor EmptyTile)
    , ((23,8), MkWall (MkWallShape Vertical))
    , ((24,8), MkFloor EmptyTile)
    , ((25,8), MkWall (MkWallShape Vertical))
    , ((26,8), MkFloor EmptyTile)
    , ((27,8), MkWall (MkWallShape Vertical))
    , ((0,9), MkWall (MkWallShape Vertical))
    , ((1,9), MkFloor EmptyTile)
    , ((2,9), MkWall (MkWallShape Vertical))
    , ((3,9), MkFloor EmptyTile)
    , ((4,9), MkWall (MkWallShape Vertical))
    , ((5,9), MkFloor EmptyTile)
    , ((6,9), MkWall (MkWallShape Vertical))
    , ((7,9), MkFloor EmptyTile)
    , ((8,9), MkFloor EmptyTile)
    , ((9,9), MkFloor EmptyTile)
    , ((10,9), MkFloor EmptyTile)
    , ((11,9), MkFloor EmptyTile)
    , ((12,9), MkFloor EmptyTile)
    , ((13,9), MkFloor EmptyTile)
    , ((14,9), MkFloor EmptyTile)
    , ((15,9), MkFloor EmptyTile)
    , ((16,9), MkFloor EmptyTile)
    , ((17,9), MkFloor EmptyTile)
    , ((18,9), MkFloor EmptyTile)
    , ((19,9), MkFloor EmptyTile)
    , ((20,9), MkFloor EmptyTile)
    , ((21,9), MkWall (MkWallShape Vertical))
    , ((22,9), MkFloor EmptyTile)
    , ((23,9), MkWall (MkWallShape Vertical))
    , ((24,9), MkFloor EmptyTile)
    , ((25,9), MkWall (MkWallShape Vertical))
    , ((26,9), MkFloor EmptyTile)
    , ((27,9), MkWall (MkWallShape Vertical))
    , ((0,10), MkWall (MkWallShape Vertical))
    , ((1,10), MkFloor EmptyTile)
    , ((2,10), MkWall (MkWallShape Vertical))
    , ((3,10), MkFloor EmptyTile)
    , ((4,10), MkWall (MkWallShape Vertical))
    , ((5,10), MkFloor EmptyTile)
    , ((6,10), MkWall (MkWallShape Vertical))
    , ((7,10), MkFloor EmptyTile)
    , ((8,10), MkFloor EmptyTile)
    , ((9,10), MkFloor EmptyTile)
    , ((10,10), MkFloor EmptyTile)
    , ((11,10), MkFloor EmptyTile)
    , ((12,10), MkFloor EmptyTile)
    , ((13,10), MkFloor EmptyTile)
    , ((14,10), MkFloor EmptyTile)
    , ((15,10), MkFloor EmptyTile)
    , ((16,10), MkFloor EmptyTile)
    , ((17,10), MkFloor EmptyTile)
    , ((18,10), MkFloor EmptyTile)
    , ((19,10), MkFloor EmptyTile)
    , ((20,10), MkFloor EmptyTile)
    , ((21,10), MkWall (MkWallShape Vertical))
    , ((22,10), MkFloor EmptyTile)
    , ((23,10), MkWall (MkWallShape Vertical))
    , ((24,10), MkFloor EmptyTile)
    , ((25,10), MkWall (MkWallShape Vertical))
    , ((26,10), MkFloor EmptyTile)
    , ((27,10), MkWall (MkWallShape Vertical))
    , ((0,11), MkWall (MkWallShape Vertical))
    , ((1,11), MkFloor EmptyTile)
    , ((2,11), MkWall (MkWallShape Vertical))
    , ((3,11), MkFloor EmptyTile)
    , ((4,11), MkWall (MkWallShape Vertical))
    , ((5,11), MkFloor EmptyTile)
    , ((6,11), MkWall (MkWallShape Vertical))
    , ((7,11), MkFloor EmptyTile)
    , ((8,11), MkFloor EmptyTile)
    , ((9,11), MkFloor EmptyTile)
    , ((10,11), MkFloor EmptyTile)
    , ((11,11), MkFloor EmptyTile)
    , ((12,11), MkFloor EmptyTile)
    , ((13,11), MkFloor EmptyTile)
    , ((14,11), MkFloor EmptyTile)
    , ((15,11), MkFloor EmptyTile)
    , ((16,11), MkFloor EmptyTile)
    , ((17,11), MkFloor EmptyTile)
    , ((18,11), MkFloor EmptyTile)
    , ((19,11), MkFloor EmptyTile)
    , ((20,11), MkFloor EmptyTile)
    , ((21,11), MkWall (MkWallShape Vertical))
    , ((22,11), MkFloor EmptyTile)
    , ((23,11), MkWall (MkWallShape Vertical))
    , ((24,11), MkFloor EmptyTile)
    , ((25,11), MkWall (MkWallShape Vertical))
    , ((26,11), MkFloor EmptyTile)
    , ((27,11), MkWall (MkWallShape Vertical))
    , ((0,12), MkWall (MkWallShape Vertical))
    , ((1,12), MkFloor EmptyTile)
    , ((2,12), MkWall (MkWallShape Vertical))
    , ((3,12), MkFloor EmptyTile)
    , ((4,12), MkWall (MkWallShape Vertical))
    , ((5,12), MkFloor EmptyTile)
    , ((6,12), MkWall (MkWallShape Vertical))
    , ((7,12), MkFloor EmptyTile)
    , ((8,12), MkFloor EmptyTile)
    , ((9,12), MkFloor EmptyTile)
    , ((10,12), MkFloor EmptyTile)
    , ((11,12), MkFloor EmptyTile)
    , ((12,12), MkFloor EmptyTile)
    , ((13,12), MkFloor EmptyTile)
    , ((14,12), MkFloor EmptyTile)
    , ((15,12), MkFloor EmptyTile)
    , ((16,12), MkFloor EmptyTile)
    , ((17,12), MkFloor EmptyTile)
    , ((18,12), MkFloor EmptyTile)
    , ((19,12), MkFloor EmptyTile)
    , ((20,12), MkFloor EmptyTile)
    , ((21,12), MkWall (MkWallShape Vertical))
    , ((22,12), MkFloor EmptyTile)
    , ((23,12), MkWall (MkWallShape Vertical))
    , ((24,12), MkFloor EmptyTile)
    , ((25,12), MkWall (MkWallShape Vertical))
    , ((26,12), MkFloor EmptyTile)
    , ((27,12), MkWall (MkWallShape Vertical))
    , ((0,13), MkWall (MkWallShape Vertical))
    , ((1,13), MkFloor EmptyTile)
    , ((2,13), MkWall (MkWallShape Vertical))
    , ((3,13), MkFloor EmptyTile)
    , ((4,13), MkWall (MkWallShape Vertical))
    , ((5,13), MkFloor EmptyTile)
    , ((6,13), MkWall (MkWallShape Vertical))
    , ((7,13), MkFloor EmptyTile)
    , ((8,13), MkFloor EmptyTile)
    , ((9,13), MkFloor EmptyTile)
    , ((10,13), MkFloor EmptyTile)
    , ((11,13), MkFloor EmptyTile)
    , ((12,13), MkFloor EmptyTile)
    , ((13,13), MkFloor EmptyTile)
    , ((14,13), MkFloor EmptyTile)
    , ((15,13), MkFloor EmptyTile)
    , ((16,13), MkFloor EmptyTile)
    , ((17,13), MkFloor EmptyTile)
    , ((18,13), MkFloor EmptyTile)
    , ((19,13), MkFloor EmptyTile)
    , ((20,13), MkFloor EmptyTile)
    , ((21,13), MkWall (MkWallShape Vertical))
    , ((22,13), MkFloor EmptyTile)
    , ((23,13), MkWall (MkWallShape Vertical))
    , ((24,13), MkFloor EmptyTile)
    , ((25,13), MkWall (MkWallShape Vertical))
    , ((26,13), MkFloor EmptyTile)
    , ((27,13), MkWall (MkWallShape Vertical))
    , ((0,14), MkWall (MkWallShape Vertical))
    , ((1,14), MkFloor EmptyTile)
    , ((2,14), MkFloor EmptyTile)
    , ((3,14), MkFloor EmptyTile)
    , ((4,14), MkFloor EmptyTile)
    , ((5,14), MkFloor EmptyTile)
    , ((6,14), MkFloor EmptyTile)
    , ((7,14), MkFloor EmptyTile)
    , ((8,14), MkFloor EmptyTile)
    , ((9,14), MkFloor EmptyTile)
    , ((10,14), MkFloor EmptyTile)
    , ((11,14), MkFloor EmptyTile)
    , ((12,14), MkFloor EmptyTile)
    , ((13,14), MkFloor EmptyTile)
    , ((14,14), MkFloor EmptyTile)
    , ((15,14), MkFloor EmptyTile)
    , ((16,14), MkFloor EmptyTile)
    , ((17,14), MkFloor EmptyTile)
    , ((18,14), MkFloor EmptyTile)
    , ((19,14), MkFloor EmptyTile)
    , ((20,14), MkFloor EmptyTile)
    , ((21,14), MkFloor EmptyTile)
    , ((22,14), MkFloor EmptyTile)
    , ((23,14), MkFloor EmptyTile)
    , ((24,14), MkFloor EmptyTile)
    , ((25,14), MkFloor EmptyTile)
    , ((26,14), MkFloor EmptyTile)
    , ((27,14), MkWall (MkWallShape Vertical))
    , ((0,15), MkWall (MkWallShape Vertical))
    , ((1,15), MkFloor EmptyTile)
    , ((2,15), MkWall (MkWallShape Vertical))
    , ((3,15), MkFloor EmptyTile)
    , ((4,15), MkWall (MkWallShape Vertical))
    , ((5,15), MkFloor EmptyTile)
    , ((6,15), MkWall (MkWallShape Vertical))
    , ((7,15), MkFloor EmptyTile)
    , ((8,15), MkFloor EmptyTile)
    , ((9,15), MkFloor EmptyTile)
    , ((10,15), MkFloor EmptyTile)
    , ((11,15), MkFloor EmptyTile)
    , ((12,15), MkFloor EmptyTile)
    , ((13,15), MkFloor EmptyTile)
    , ((14,15), MkFloor EmptyTile)
    , ((15,15), MkFloor EmptyTile)
    , ((16,15), MkFloor EmptyTile)
    , ((17,15), MkFloor EmptyTile)
    , ((18,15), MkFloor EmptyTile)
    , ((19,15), MkFloor EmptyTile)
    , ((20,15), MkFloor EmptyTile)
    , ((21,15), MkWall (MkWallShape Vertical))
    , ((22,15), MkFloor EmptyTile)
    , ((23,15), MkWall (MkWallShape Vertical))
    , ((24,15), MkFloor EmptyTile)
    , ((25,15), MkWall (MkWallShape Vertical))
    , ((26,15), MkFloor EmptyTile)
    , ((27,15), MkWall (MkWallShape Vertical))
    , ((0,16), MkWall (MkWallShape Vertical))
    , ((1,16), MkFloor EmptyTile)
    , ((2,16), MkWall (MkWallShape Vertical))
    , ((3,16), MkFloor EmptyTile)
    , ((4,16), MkWall (MkWallShape Vertical))
    , ((5,16), MkFloor EmptyTile)
    , ((6,16), MkWall (MkWallShape Vertical))
    , ((7,16), MkFloor EmptyTile)
    , ((8,16), MkFloor EmptyTile)
    , ((9,16), MkFloor EmptyTile)
    , ((10,16), MkFloor EmptyTile)
    , ((11,16), MkFloor EmptyTile)
    , ((12,16), MkFloor EmptyTile)
    , ((13,16), MkFloor EmptyTile)
    , ((14,16), MkFloor EmptyTile)
    , ((15,16), MkFloor EmptyTile)
    , ((16,16), MkFloor EmptyTile)
    , ((17,16), MkFloor EmptyTile)
    , ((18,16), MkFloor EmptyTile)
    , ((19,16), MkFloor EmptyTile)
    , ((20,16), MkFloor EmptyTile)
    , ((21,16), MkWall (MkWallShape Vertical))
    , ((22,16), MkFloor EmptyTile)
    , ((23,16), MkWall (MkWallShape Vertical))
    , ((24,16), MkFloor EmptyTile)
    , ((25,16), MkWall (MkWallShape Vertical))
    , ((26,16), MkFloor EmptyTile)
    , ((27,16), MkWall (MkWallShape Vertical))
    , ((0,17), MkWall (MkWallShape Vertical))
    , ((1,17), MkFloor EmptyTile)
    , ((2,17), MkWall (MkWallShape Vertical))
    , ((3,17), MkFloor EmptyTile)
    , ((4,17), MkWall (MkWallShape Vertical))
    , ((5,17), MkFloor EmptyTile)
    , ((6,17), MkWall (MkWallShape Vertical))
    , ((7,17), MkFloor EmptyTile)
    , ((8,17), MkFloor EmptyTile)
    , ((9,17), MkFloor EmptyTile)
    , ((10,17), MkFloor EmptyTile)
    , ((11,17), MkFloor EmptyTile)
    , ((12,17), MkFloor EmptyTile)
    , ((13,17), MkFloor EmptyTile)
    , ((14,17), MkFloor EmptyTile)
    , ((15,17), MkFloor EmptyTile)
    , ((16,17), MkFloor EmptyTile)
    , ((17,17), MkFloor EmptyTile)
    , ((18,17), MkFloor EmptyTile)
    , ((19,17), MkFloor EmptyTile)
    , ((20,17), MkFloor EmptyTile)
    , ((21,17), MkWall (MkWallShape Vertical))
    , ((22,17), MkFloor EmptyTile)
    , ((23,17), MkWall (MkWallShape Vertical))
    , ((24,17), MkFloor EmptyTile)
    , ((25,17), MkWall (MkWallShape Vertical))
    , ((26,17), MkFloor EmptyTile)
    , ((27,17), MkWall (MkWallShape Vertical))
    , ((0,18), MkWall (MkWallShape Vertical))
    , ((1,18), MkFloor EmptyTile)
    , ((2,18), MkWall (MkWallShape Vertical))
    , ((3,18), MkFloor EmptyTile)
    , ((4,18), MkWall (MkWallShape Vertical))
    , ((5,18), MkFloor EmptyTile)
    , ((6,18), MkWall (MkWallShape Vertical))
    , ((7,18), MkFloor EmptyTile)
    , ((8,18), MkFloor EmptyTile)
    , ((9,18), MkFloor EmptyTile)
    , ((10,18), MkFloor EmptyTile)
    , ((11,18), MkFloor EmptyTile)
    , ((12,18), MkFloor EmptyTile)
    , ((13,18), MkFloor EmptyTile)
    , ((14,18), MkFloor EmptyTile)
    , ((15,18), MkFloor EmptyTile)
    , ((16,18), MkFloor EmptyTile)
    , ((17,18), MkFloor EmptyTile)
    , ((18,18), MkFloor EmptyTile)
    , ((19,18), MkFloor EmptyTile)
    , ((20,18), MkFloor EmptyTile)
    , ((21,18), MkWall (MkWallShape Vertical))
    , ((22,18), MkFloor EmptyTile)
    , ((23,18), MkWall (MkWallShape Vertical))
    , ((24,18), MkFloor EmptyTile)
    , ((25,18), MkWall (MkWallShape Vertical))
    , ((26,18), MkFloor EmptyTile)
    , ((27,18), MkWall (MkWallShape Vertical))
    , ((0,19), MkWall (MkWallShape Vertical))
    , ((1,19), MkFloor EmptyTile)
    , ((2,19), MkWall (MkWallShape Vertical))
    , ((3,19), MkFloor EmptyTile)
    , ((4,19), MkWall (MkWallShape Vertical))
    , ((5,19), MkFloor EmptyTile)
    , ((6,19), MkWall (MkWallShape Vertical))
    , ((7,19), MkFloor EmptyTile)
    , ((8,19), MkFloor EmptyTile)
    , ((9,19), MkFloor EmptyTile)
    , ((10,19), MkFloor EmptyTile)
    , ((11,19), MkFloor EmptyTile)
    , ((12,19), MkFloor EmptyTile)
    , ((13,19), MkFloor EmptyTile)
    , ((14,19), MkFloor EmptyTile)
    , ((15,19), MkFloor EmptyTile)
    , ((16,19), MkFloor EmptyTile)
    , ((17,19), MkFloor EmptyTile)
    , ((18,19), MkFloor EmptyTile)
    , ((19,19), MkFloor EmptyTile)
    , ((20,19), MkFloor EmptyTile)
    , ((21,19), MkWall (MkWallShape Vertical))
    , ((22,19), MkFloor EmptyTile)
    , ((23,19), MkWall (MkWallShape Vertical))
    , ((24,19), MkFloor EmptyTile)
    , ((25,19), MkWall (MkWallShape Vertical))
    , ((26,19), MkFloor EmptyTile)
    , ((27,19), MkWall (MkWallShape Vertical))
    , ((0,20), MkWall (MkWallShape Vertical))
    , ((1,20), MkFloor EmptyTile)
    , ((2,20), MkWall (MkWallShape Vertical))
    , ((3,20), MkFloor EmptyTile)
    , ((4,20), MkWall (MkWallShape Vertical))
    , ((5,20), MkFloor EmptyTile)
    , ((6,20), MkWall (MkWallShape Vertical))
    , ((7,20), MkFloor EmptyTile)
    , ((8,20), MkFloor EmptyTile)
    , ((9,20), MkFloor EmptyTile)
    , ((10,20), MkFloor EmptyTile)
    , ((11,20), MkFloor EmptyTile)
    , ((12,20), MkFloor EmptyTile)
    , ((13,20), MkFloor EmptyTile)
    , ((14,20), MkFloor EmptyTile)
    , ((15,20), MkFloor EmptyTile)
    , ((16,20), MkFloor EmptyTile)
    , ((17,20), MkFloor EmptyTile)
    , ((18,20), MkFloor EmptyTile)
    , ((19,20), MkFloor EmptyTile)
    , ((20,20), MkFloor EmptyTile)
    , ((21,20), MkWall (MkWallShape Vertical))
    , ((22,20), MkFloor EmptyTile)
    , ((23,20), MkWall (MkWallShape Vertical))
    , ((24,20), MkFloor EmptyTile)
    , ((25,20), MkWall (MkWallShape Vertical))
    , ((26,20), MkFloor EmptyTile)
    , ((27,20), MkWall (MkWallShape Vertical))
    , ((0,21), MkWall (MkWallShape Vertical))
    , ((1,21), MkFloor EmptyTile)
    , ((2,21), MkWall (MkWallShape Vertical))
    , ((3,21), MkFloor EmptyTile)
    , ((4,21), MkWall (MkWallShape Vertical))
    , ((5,21), MkFloor EmptyTile)
    , ((6,21), MkWall (MkWallShape Vertical))
    , ((7,21), MkFloor EmptyTile)
    , ((8,21), MkFloor EmptyTile)
    , ((9,21), MkFloor EmptyTile)
    , ((10,21), MkFloor EmptyTile)
    , ((11,21), MkFloor EmptyTile)
    , ((12,21), MkFloor EmptyTile)
    , ((13,21), MkFloor EmptyTile)
    , ((14,21), MkFloor EmptyTile)
    , ((15,21), MkFloor EmptyTile)
    , ((16,21), MkFloor EmptyTile)
    , ((17,21), MkFloor EmptyTile)
    , ((18,21), MkFloor EmptyTile)
    , ((19,21), MkFloor EmptyTile)
    , ((20,21), MkFloor EmptyTile)
    , ((21,21), MkWall (MkWallShape Vertical))
    , ((22,21), MkFloor EmptyTile)
    , ((23,21), MkWall (MkWallShape Vertical))
    , ((24,21), MkFloor EmptyTile)
    , ((25,21), MkWall (MkWallShape Vertical))
    , ((26,21), MkFloor EmptyTile)
    , ((27,21), MkWall (MkWallShape Vertical))
    , ((0,22), MkWall (MkWallShape Vertical))
    , ((1,22), MkFloor EmptyTile)
    , ((2,22), MkWall (MkWallShape Vertical))
    , ((3,22), MkFloor EmptyTile)
    , ((4,22), MkWall (MkWallShape Vertical))
    , ((5,22), MkFloor EmptyTile)
    , ((6,22), MkWall (MkWallShape Vertical))
    , ((7,22), MkWall (MkWallShape Vertical))
    , ((8,22), MkWall (MkWallShape Vertical))
    , ((9,22), MkWall (MkWallShape Vertical))
    , ((10,22), MkWall (MkWallShape Vertical))
    , ((11,22), MkWall (MkWallShape Vertical))
    , ((12,22), MkWall (MkWallShape Vertical))
    , ((13,22), MkFloor EmptyTile)
    , ((14,22), MkWall (MkWallShape Vertical))
    , ((15,22), MkWall (MkWallShape Vertical))
    , ((16,22), MkWall (MkWallShape Vertical))
    , ((17,22), MkWall (MkWallShape Vertical))
    , ((18,22), MkWall (MkWallShape Vertical))
    , ((19,22), MkWall (MkWallShape Vertical))
    , ((20,22), MkWall (MkWallShape Vertical))
    , ((21,22), MkWall (MkWallShape Vertical))
    , ((22,22), MkFloor EmptyTile)
    , ((23,22), MkWall (MkWallShape Vertical))
    , ((24,22), MkFloor EmptyTile)
    , ((25,22), MkWall (MkWallShape Vertical))
    , ((26,22), MkFloor EmptyTile)
    , ((27,22), MkWall (MkWallShape Vertical))
    , ((0,23), MkWall (MkWallShape Vertical))
    , ((1,23), MkFloor EmptyTile)
    , ((2,23), MkWall (MkWallShape Vertical))
    , ((3,23), MkFloor EmptyTile)
    , ((4,23), MkWall (MkWallShape Vertical))
    , ((5,23), MkFloor EmptyTile)
    , ((6,23), MkFloor EmptyTile)
    , ((7,23), MkFloor EmptyTile)
    , ((8,23), MkFloor EmptyTile)
    , ((9,23), MkFloor EmptyTile)
    , ((10,23), MkFloor EmptyTile)
    , ((11,23), MkFloor EmptyTile)
    , ((12,23), MkFloor EmptyTile)
    , ((13,23), MkFloor EmptyTile)
    , ((14,23), MkFloor EmptyTile)
    , ((15,23), MkFloor EmptyTile)
    , ((16,23), MkFloor EmptyTile)
    , ((17,23), MkFloor EmptyTile)
    , ((18,23), MkFloor EmptyTile)
    , ((19,23), MkFloor EmptyTile)
    , ((20,23), MkFloor EmptyTile)
    , ((21,23), MkFloor EmptyTile)
    , ((22,23), MkFloor EmptyTile)
    , ((23,23), MkWall (MkWallShape Vertical))
    , ((24,23), MkFloor EmptyTile)
    , ((25,23), MkWall (MkWallShape Vertical))
    , ((26,23), MkFloor EmptyTile)
    , ((27,23), MkWall (MkWallShape Vertical))
    , ((0,24), MkWall (MkWallShape Vertical))
    , ((1,24), MkFloor EmptyTile)
    , ((2,24), MkWall (MkWallShape Vertical))
    , ((3,24), MkFloor EmptyTile)
    , ((4,24), MkWall (MkWallShape Vertical))
    , ((5,24), MkWall (MkWallShape Vertical))
    , ((6,24), MkWall (MkWallShape Vertical))
    , ((7,24), MkWall (MkWallShape Vertical))
    , ((8,24), MkWall (MkWallShape Vertical))
    , ((9,24), MkWall (MkWallShape Vertical))
    , ((10,24), MkWall (MkWallShape Vertical))
    , ((11,24), MkWall (MkWallShape Vertical))
    , ((12,24), MkWall (MkWallShape Vertical))
    , ((13,24), MkFloor EmptyTile)
    , ((14,24), MkWall (MkWallShape Vertical))
    , ((15,24), MkWall (MkWallShape Vertical))
    , ((16,24), MkWall (MkWallShape Vertical))
    , ((17,24), MkWall (MkWallShape Vertical))
    , ((18,24), MkWall (MkWallShape Vertical))
    , ((19,24), MkWall (MkWallShape Vertical))
    , ((20,24), MkWall (MkWallShape Vertical))
    , ((21,24), MkWall (MkWallShape Vertical))
    , ((22,24), MkWall (MkWallShape Vertical))
    , ((23,24), MkWall (MkWallShape Vertical))
    , ((24,24), MkFloor EmptyTile)
    , ((25,24), MkWall (MkWallShape Vertical))
    , ((26,24), MkFloor EmptyTile)
    , ((27,24), MkWall (MkWallShape Vertical))
    , ((0,25), MkWall (MkWallShape Vertical))
    , ((1,25), MkFloor EmptyTile)
    , ((2,25), MkWall (MkWallShape Vertical))
    , ((3,25), MkFloor EmptyTile)
    , ((4,25), MkFloor EmptyTile)
    , ((5,25), MkFloor EmptyTile)
    , ((6,25), MkFloor EmptyTile)
    , ((7,25), MkFloor EmptyTile)
    , ((8,25), MkFloor EmptyTile)
    , ((9,25), MkFloor EmptyTile)
    , ((10,25), MkFloor EmptyTile)
    , ((11,25), MkFloor EmptyTile)
    , ((12,25), MkFloor EmptyTile)
    , ((13,25), MkFloor EmptyTile)
    , ((14,25), MkFloor EmptyTile)
    , ((15,25), MkFloor EmptyTile)
    , ((16,25), MkFloor EmptyTile)
    , ((17,25), MkFloor EmptyTile)
    , ((18,25), MkFloor EmptyTile)
    , ((19,25), MkFloor EmptyTile)
    , ((20,25), MkFloor EmptyTile)
    , ((21,25), MkFloor EmptyTile)
    , ((22,25), MkFloor EmptyTile)
    , ((23,25), MkFloor EmptyTile)
    , ((24,25), MkFloor EmptyTile)
    , ((25,25), MkWall (MkWallShape Vertical))
    , ((26,25), MkFloor EmptyTile)
    , ((27,25), MkWall (MkWallShape Vertical))
    , ((0,26), MkWall (MkWallShape Vertical))
    , ((1,26), MkFloor EmptyTile)
    , ((2,26), MkWall (MkWallShape Vertical))
    , ((3,26), MkWall (MkWallShape Vertical))
    , ((4,26), MkWall (MkWallShape Vertical))
    , ((5,26), MkWall (MkWallShape Vertical))
    , ((6,26), MkWall (MkWallShape Vertical))
    , ((7,26), MkWall (MkWallShape Vertical))
    , ((8,26), MkWall (MkWallShape Vertical))
    , ((9,26), MkWall (MkWallShape Vertical))
    , ((10,26), MkWall (MkWallShape Vertical))
    , ((11,26), MkWall (MkWallShape Vertical))
    , ((12,26), MkWall (MkWallShape Vertical))
    , ((13,26), MkFloor EmptyTile)
    , ((14,26), MkWall (MkWallShape Vertical))
    , ((15,26), MkWall (MkWallShape Vertical))
    , ((16,26), MkWall (MkWallShape Vertical))
    , ((17,26), MkWall (MkWallShape Vertical))
    , ((18,26), MkWall (MkWallShape Vertical))
    , ((19,26), MkWall (MkWallShape Vertical))
    , ((20,26), MkWall (MkWallShape Vertical))
    , ((21,26), MkWall (MkWallShape Vertical))
    , ((22,26), MkWall (MkWallShape Vertical))
    , ((23,26), MkWall (MkWallShape Vertical))
    , ((24,26), MkWall (MkWallShape Vertical))
    , ((25,26), MkWall (MkWallShape Vertical))
    , ((26,26), MkFloor EmptyTile)
    , ((27,26), MkWall (MkWallShape Vertical))
    , ((0,27), MkWall (MkWallShape Vertical))
    , ((1,27), MkFloor EmptyTile)
    , ((2,27), MkFloor EmptyTile)
    , ((3,27), MkFloor EmptyTile)
    , ((4,27), MkFloor EmptyTile)
    , ((5,27), MkFloor EmptyTile)
    , ((6,27), MkFloor EmptyTile)
    , ((7,27), MkFloor EmptyTile)
    , ((8,27), MkFloor EmptyTile)
    , ((9,27), MkFloor EmptyTile)
    , ((10,27), MkFloor EmptyTile)
    , ((11,27), MkFloor EmptyTile)
    , ((12,27), MkFloor EmptyTile)
    , ((13,27), MkFloor EmptyTile)
    , ((14,27), MkFloor EmptyTile)
    , ((15,27), MkFloor EmptyTile)
    , ((16,27), MkFloor EmptyTile)
    , ((17,27), MkFloor EmptyTile)
    , ((18,27), MkFloor EmptyTile)
    , ((19,27), MkFloor EmptyTile)
    , ((20,27), MkFloor EmptyTile)
    , ((21,27), MkFloor EmptyTile)
    , ((22,27), MkFloor EmptyTile)
    , ((23,27), MkFloor EmptyTile)
    , ((24,27), MkFloor EmptyTile)
    , ((25,27), MkFloor EmptyTile)
    , ((26,27), MkFloor EmptyTile)
    , ((27,27), MkWall (MkWallShape Vertical))
    , ((0,28), MkWall (MkWallShape Vertical))
    , ((1,28), MkWall (MkWallShape Vertical))
    , ((2,28), MkWall (MkWallShape Vertical))
    , ((3,28), MkWall (MkWallShape Vertical))
    , ((4,28), MkWall (MkWallShape Vertical))
    , ((5,28), MkWall (MkWallShape Vertical))
    , ((6,28), MkWall (MkWallShape Vertical))
    , ((7,28), MkWall (MkWallShape Vertical))
    , ((8,28), MkWall (MkWallShape Vertical))
    , ((9,28), MkWall (MkWallShape Vertical))
    , ((10,28), MkWall (MkWallShape Vertical))
    , ((11,28), MkWall (MkWallShape Vertical))
    , ((12,28), MkWall (MkWallShape Vertical))
    , ((13,28), MkWall (MkWallShape Vertical))
    , ((14,28), MkWall (MkWallShape Vertical))
    , ((15,28), MkWall (MkWallShape Vertical))
    , ((16,28), MkWall (MkWallShape Vertical))
    , ((17,28), MkWall (MkWallShape Vertical))
    , ((18,28), MkWall (MkWallShape Vertical))
    , ((19,28), MkWall (MkWallShape Vertical))
    , ((20,28), MkWall (MkWallShape Vertical))
    , ((21,28), MkWall (MkWallShape Vertical))
    , ((22,28), MkWall (MkWallShape Vertical))
    , ((23,28), MkWall (MkWallShape Vertical))
    , ((24,28), MkWall (MkWallShape Vertical))
    , ((25,28), MkWall (MkWallShape Vertical))
    , ((26,28), MkWall (MkWallShape Vertical))
    , ((27,28), MkWall (MkWallShape Vertical))
    ]
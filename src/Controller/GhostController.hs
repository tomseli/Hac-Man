module Controller.GhostController where

import Controller.EntityController
import Data.List (minimumBy)
import Data.Ord (comparing)
import Model.Entities
import Model.Maze

listOfDirections :: [Direction]
listOfDirections =
  [ Model.Entities.Left
  , Model.Entities.Right
  , Model.Entities.Up
  , Model.Entities.Down
  ]

getValidDirections :: Entity -> Maze -> [Direction]
getValidDirections ent maze = [dir | dir <- listOfDirections, valid dir && invalid dir]
 where
  valid dir = case checkEntCollision checkWall (changeDirEnt ent dir) 1 maze of
    Nothing -> True
    Just _ -> False
  invalid dir = dir /= (direction . movement) ent

chooseDirection :: [Direction] -> EntityPosition -> EntityPosition -> Direction
chooseDirection xs (x, y) (x', y') =
  minimumBy
    (comparing (\dir -> distanceTilePos (getNextPos (x, y) dir 1.0) (x', y')))
    xs

distanceTilePos :: EntityPosition -> EntityPosition -> Float
distanceTilePos (x, y) (x', y') = ((x - x') * (x - x')) + ((y - y') * (y - y'))

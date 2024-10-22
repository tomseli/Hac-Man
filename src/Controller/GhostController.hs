{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Controller.GhostController where

import Controller.EntityController
import Data.List (minimumBy)
import Data.Ord (comparing)
import Model.Entities
import Model.Maze (Maze)
import Model.Model

listOfDirections :: [Direction]
listOfDirections =
  [ Model.Entities.Left
  , Model.Entities.Right
  , Model.Entities.Up
  , Model.Entities.Down
  ]

getValidDirections :: Entity -> Maze -> [Direction]
getValidDirections ent maz = [dir | dir <- listOfDirections, valid dir && not (invalid dir)]
 where
  valid dir = case checkEntCollision checkWall (changeDirEnt ent dir) 1 maz of
    Nothing -> True
    Just _ -> False
  invalid dir = dir ==  getOpDirection ent ((direction.movement) ent)

chooseDirection :: Entity -> [Direction] -> EntityPosition -> EntityPosition -> Direction
chooseDirection ent []  _ _ = (direction.movement) ent --go same direction
chooseDirection _ xs (x, y) (x', y') =
  minimumBy
    (comparing (\dir -> distanceTilePos (getNextPos (x, y) dir 1.0) (x', y')))
    xs

distanceTilePos :: EntityPosition -> EntityPosition -> Float
distanceTilePos (x, y) (x', y') = ((x - x') * (x - x')) + ((y - y') * (y - y'))

moveGhost :: GameState -> Entity -> Float -> Maze -> Entity
moveGhost state ent dis maz = moveWithCollision (changeHeadingEnt ent{oldDirection = (direction.movement) ent} decision) dis maz
    where
      decision = chooseDirection ent (getValidDirections ent maz) ((position.movement) ent) ((position.movement.entity.player) state)

getOpDirection :: Entity -> Direction -> Direction
getOpDirection _ Model.Entities.Left = Model.Entities.Right
getOpDirection _ Model.Entities.Right = Model.Entities.Left
getOpDirection _ Model.Entities.Up = Model.Entities.Down
getOpDirection _ Model.Entities.Down = Model.Entities.Up
getOpDirection ent Model.Entities.Still = getOpDirection ent $ oldDirection ent




      -- listOfDirections :: [Direction]

-- listOfDirections =
--   [ Model.Entities.Left
--   , Model.Entities.Right
--   , Model.Entities.Up
--   , Model.Entities.Down
--   ]

-- getValidDirections :: Entity -> Maze -> [Direction]
-- getValidDirections ent maz = [dir | dir <- listOfDirections, valid dir && invalid dir]
--  where
--   valid dir = case checkEntCollision checkWall (changeDirEnt ent dir) 1 maz of
--     Nothing -> False
--     Just _ -> True
--   invalid dir = dir /= getOpDirection ((direction . movement) ent)

-- chooseDirection :: [Direction] -> EntityPosition -> EntityPosition -> Direction
-- chooseDirection [] _ _ = error "stuck"
-- chooseDirection xs (x, y) (x', y') =
--   minimumBy
--     (comparing (\dir -> distanceTilePos (getNextPos (x, y) dir 1.0) (x', y')))
--     xs


-- distanceTilePos :: EntityPosition -> EntityPosition -> Float
-- distanceTilePos (x, y) (x', y') = ((x - x') * (x - x')) + ((y - y') * (y - y'))

-- moveGhost :: GameState -> Entity -> Float -> Maze -> Entity
-- moveGhost state ent dis maz = moveWithCollision (changeHeadingEnt ent decision) dis maz
--     where
--       decision = chooseDirection (getValidDirections ent maz) ((position.movement) ent) (2, -2)--((position.movement.entity.player) state)

-- getOpDirection :: Direction -> Direction
-- getOpDirection Model.Entities.Left = Model.Entities.Right
-- getOpDirection Model.Entities.Right = Model.Entities.Left
-- getOpDirection Model.Entities.Up = Model.Entities.Down
-- getOpDirection Model.Entities.Down = Model.Entities.Up
-- getOpDirection Model.Entities.Still = Model.Entities.Still
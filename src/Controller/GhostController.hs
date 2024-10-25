{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Controller.GhostController where

import           Control.Monad.State         (State, evalState, state)

import           Controller.EntityController

import           Data.List                   (minimumBy)
import           Data.Ord                    (comparing)

import           Model.Entities
import           Model.Maze                  (Maze)
import           Model.Model

import           System.Random               (StdGen, mkStdGen, randomR)


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
    Just _  -> False
  invalid dir = dir ==  getOpDirection ent ((direction.movement) ent)

chooseDirection :: Entity -> [Direction] -> EntityPosition -> EntityPosition -> Direction
chooseDirection ent []  _ _ = (direction.movement) ent --go same direction
chooseDirection _ xs (x, y) (x', y') =
  minimumBy
    (comparing (\dir -> distanceTilePos (getNextPos (x, y) dir 1.0) (x', y')))
    xs

--dia 29 zegt dat je de gen er al uit kan halen, snap niet waarom (lambda?)
--unsafe: if ghost has no valid directions
chooseDirectionFrightened :: [Direction]  -> State StdGen Direction
chooseDirectionFrightened directions = Control.Monad.State.state $ \gen ->
  let (index, nGen) = randomR (0, length directions - 1) gen
  in (directions !! index, nGen)


distanceTilePos :: EntityPosition -> EntityPosition -> Float
distanceTilePos (x, y) (x', y') = ((x - x') * (x - x')) + ((y - y') * (y - y'))

moveGhost :: GameState -> Ghost -> Float -> Maze -> Entity
moveGhost _ ghost@MkGhost{entityG = ent} dis maz = moveWithCollision (checkValidHeading (changeHeadingEnt ent{oldDirection =
  (direction.movement) ent} decision) 0.008 maz) dis maz
    where
      decision = moveGhost' ent ghost maz

moveGhost' :: Entity -> Ghost -> Maze -> Direction
moveGhost' ent ghost maz  = direction
  where
    gen = mkStdGen 42
    target = case behaviourMode ghost of
            Chase      -> targetTile ghost
            Scatter    -> homeCorner ghost
            Frightened -> targetTile ghost
            _          -> homeCorner ghost
    direction = case behaviourMode ghost of
              Frightened -> evalState (chooseDirectionFrightened (getValidDirections ent maz)) gen -- return direction
              _          -> chooseDirection ent (getValidDirections ent maz) ((position.movement) ent) target

updateGhostPositions :: [Ghost] -> GameState -> [Ghost]
updateGhostPositions [] _      = []
updateGhostPositions xs gstate = [updateGhostPositions' x gstate | x <- xs]

updateGhostPositions' :: Ghost -> GameState -> Ghost
updateGhostPositions' gh@MkGhost{ghostName = Blinky} gstate = gh{targetTile = (position.movement.entity.player) gstate}
updateGhostPositions' g _      = g




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


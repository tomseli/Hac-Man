{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Controller.GhostController where

import           Control.Monad.State         (State, evalState, state)

import           Controller.EntityController

import           Data.List                   (find, minimumBy)
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
  , Model.Entities.Still
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
            Chase _      -> targetTile ghost
            Scatter _    -> homeCorner ghost
            Frightened _ -> targetTile ghost
            _            -> homeCorner ghost
    direction = case behaviourMode ghost of
              Frightened _-> evalState (chooseDirectionFrightened (getValidDirections ent maz)) gen -- return direction
              _          -> chooseDirection ent (getValidDirections ent maz) ((position.movement) ent) target

updateGhostPositions :: [Ghost] -> GameState -> [Ghost]
updateGhostPositions [] _      = []
updateGhostPositions xs gstate = [updateGhostPositions' x gstate | x <- xs]

updateGhostPositions' :: Ghost -> GameState -> Ghost
updateGhostPositions' gh@MkGhost{ghostName = Blinky} gstate = gh{targetTile = (position.movement.entity.player) gstate}
updateGhostPositions' g _      = g

--include a better function for handling a hit
checkGhosts :: GameState -> GameState
checkGhosts gstate@MkGameState{ghosts = xs, player = p} =
  case hitGhost of
    Just ghost -> handleGhostInteraction gstate ghost
    Nothing    -> gstate
  where
    -- Find the first ghost that matches the player's position
    hitGhost = find (\x -> snapToGrid (position . movement . entity $ p) == snapToGrid (position . movement . entityG $ x)) xs

-- checkifScatter :: GameState -> GameState
-- checkifScatter gstate@MkGameState{ghosts = xs} | any f xs  = gstate{ghosts = changeGhostBehaviour xs (Scatter 7)}
--                                                | otherwise = gstate
--   where
--     f x = elapsedTime gstate > extractTime (behaviourMode x)


gotoScatterGhosts :: GameState -> GameState
gotoScatterGhosts gstate@MkGameState{ghosts, elapsedTime}
    | extractTime (behaviourMode (head ghosts)) < elapsedTime =
        gstate { ghosts = changeGhostBehaviour ghosts newBehaviour }
    | otherwise = gstate
  where
    isChasing = case behaviourMode (head ghosts) of
                  Chase _ -> True
                  _       -> False
    newBehaviour = if isChasing then Scatter (elapsedTime + 7) else Chase (elapsedTime + 20) -- could add extra check to max to four scatters

extractTime :: BehaviourMode -> Float
extractTime (Chase time)      = time
extractTime (Scatter time)    = time
extractTime (Frightened time) = time
extractTime (Home time)       = time

resetEntityPos :: Entity -> EntityPosition -> Entity
resetEntityPos ent@MkEntity{movement = move} (x, y) = ent{movement = move{position = (x, y)}}

handleGhostInteraction :: GameState -> Ghost -> GameState
handleGhostInteraction gstate@MkGameState{ghosts = xs, player = p} g
  | isFrightened g = gstate {
      ghosts = updateCorrectGhost,
      player = p{score = score p + 400}
    }
  | otherwise =  checkPlayerDeath gstate{ player = resetPlayer, ghosts = resetAllGhosts } p
  where
    -- Reset only the specific ghost that was hit
    updateCorrectGhost = map (\ghost -> if ghost == g then resetGhost' g else ghost) xs
    -- Reset all ghosts if the player is not invincible (not frightened state)
    resetAllGhosts = resetGhost xs
    -- Reset player position and direction to ensure consistency
    resetPlayer = changeDirPlayer (p { entity = resetEntityPos (entity p) (2, -2), lives = lives p -1}) Still


resetGhost :: [Ghost] -> [Ghost]
resetGhost xs = [resetGhost' x | x <- xs]

resetGhost' :: Ghost -> Ghost
resetGhost' ghost@MkGhost{ghostName = Blinky, entityG} = ghost{entityG = resetEntityPos entityG (27, -2)}
resetGhost' ghost@MkGhost{ghostName = _     , entityG} = ghost{entityG = resetEntityPos entityG (0, 0)}

-- chech ghost for Frigtend ignoring the time paramter
isFrightened :: Ghost -> Bool
isFrightened ghost =
  case behaviourMode ghost of
  Frightened _ -> True
  _            -> False


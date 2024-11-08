{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Controller.GhostController where

import           Control.Monad.State         (State, evalState, state)

import           Controller.EntityController

import           Data.List                   (find, minimumBy)
import           Data.Ord                    (comparing)

import           Model.Entities
import           Model.Maze
import           Model.Model

import           System.Random               (StdGen, mkStdGen, randomR)

import           View.Transform              (tileSize)


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
moveGhost _ ghost@MkGhost{entityG = ent} dis maz | not (disAbleMove ghost) =
  moveWithCollision (checkValidHeading (changeHeadingEnt ent{oldDirection =
  (direction.movement) ent} decision) 0.008 maz) dis maz
                                                 | otherwise = ent
    where
      decision = moveGhost' ent ghost maz

moveGhost' :: Entity -> Ghost -> Maze -> Direction
moveGhost' ent ghost maz  = direction
  where
    gen = mkStdGen 42
    target = case behaviourMode ghost of
            Chase _      -> targetTile ghost
            Scatter _    -> scatterCorner ghost
            Frightened _ -> targetTile ghost
            Home _       -> homeTile ghost
    direction = case behaviourMode ghost of
              Frightened _-> evalState (chooseDirectionFrightened (getValidDirections ent maz)) gen -- return direction
              _          -> chooseDirection ent (getValidDirections ent maz) ((position.movement) ent) target

updateGhostPositions :: [Ghost] -> GameState -> [Ghost]
updateGhostPositions [] _      = []
updateGhostPositions xs gstate = [updateGhostPositions' x gstate | x <- xs]

--implement target tile pos algorithms here
updateGhostPositions' :: Ghost -> GameState -> Ghost
updateGhostPositions' gh@MkGhost{ghostName = Blinky} gstate = gh{targetTile = (position.movement.entity.player) gstate}
updateGhostPositions' gh@MkGhost{ghostName = Pinky} gstate = gh{targetTile = (getNextPos $ (position.movement.entity.player) gstate) ((direction.movement.entity.player) gstate) 4}
updateGhostPositions' gh@MkGhost{ghostName = Clyde} gstate = tilePositionClyde (player gstate) gh
updateGhostPositions' g _      = g

--not the most beautifull solution to get the correct tiledistance
-- not sure if this ACTUALLY the right algorithm for clyde but close enough ig
tilePositionClyde :: Player -> Ghost -> Ghost
tilePositionClyde player ghost | distanceTilePos playerPos ghostPos <= (4 * fst tileSize) =  ghost{targetTile = scatterCorner ghost}
                               | otherwise = ghost{targetTile = playerPos}
                                where playerPos =  (position.movement.entity) player
                                      ghostPos =  (position.movement.entityG) ghost



--include a better function for handling a hit
checkGhosts :: GameState -> GameState
checkGhosts gstate@MkGameState{ghosts = xs, player = p} =
  case hitGhost of
    Just ghost -> handleGhostInteraction gstate ghost
    Nothing    -> gstate
  where
    -- Find the first ghost that matches the player's position
    hitGhost = find (\x -> snapToGrid (position . movement . entity $ p) == snapToGrid (position . movement . entityG $ x)) xs


gotoScatterGhosts :: GameState -> GameState
gotoScatterGhosts gstate@MkGameState{ghosts, elapsedTime} =
    gstate { ghosts = map updateGhostBehaviour ghosts }
  where
    updateGhostBehaviour ghost
      | extractTime (behaviourMode ghost) < elapsedTime = ghost { behaviourMode = toggleBehaviour (behaviourMode ghost) }
      | otherwise = ghost

    toggleBehaviour (Chase _)   = Scatter (elapsedTime + 7)
    toggleBehaviour (Scatter _) = Chase (elapsedTime + 20)
    toggleBehaviour (Home _)    = Scatter (elapsedTime + 7)
    toggleBehaviour _           = Chase (elapsedTime + 20)


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
    updateCorrectGhost = map (\ghost -> if ghost == g then resetGhost' gstate g else ghost) xs
    -- Reset all ghosts if the player is not invincible (not frightened state)
    resetAllGhosts = resetGhost gstate xs
    -- Reset player position and direction to ensure consistency
    resetPlayer = changeDirPlayer (p { entity = resetEntityPos (entity p) (2, -2), lives = lives p -1}) Still


resetGhost :: GameState -> [Ghost] -> [Ghost]
resetGhost gstate xs = [resetGhost' gstate x | x <- xs]

resetGhost' ::  GameState -> Ghost -> Ghost
resetGhost' gstate ghost@MkGhost{entityG} =
   disableMovement ghost{entityG = resetEntityPos entityG (homeTile ghost),
                         behaviourMode = Home (elapsedTime gstate + 0)} -- stay in home for a second

enableMovement :: Ghost -> Ghost
enableMovement g = g{disAbleMove = False}

disableMovement :: Ghost -> Ghost
disableMovement g = g{disAbleMove = True}

-- chech ghost for Frigtend ignoring the time paramter
isFrightened :: Ghost -> Bool
isFrightened ghost =
  case behaviourMode ghost of
  Frightened _ -> True
  _            -> False

--debug function for printing all ghosts
printActiveGhosts :: GameState -> IO ()
printActiveGhosts gstate =  putStrLn $ unwords (map (show . ghostName) (ghosts gstate))

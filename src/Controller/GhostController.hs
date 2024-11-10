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

chooseDirectionFrightened :: [Direction]  -> State StdGen Direction
chooseDirectionFrightened directions = Control.Monad.State.state $ \gen ->
  let (index, nGen) = randomR (0, length directions - 1) gen
  in if index > -1 then (directions !! index, nGen)
       else (Model.Entities.Down, nGen) -- if ghosts have no valid direction, go down (arbitrary)

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
              _           -> chooseDirection ent (getValidDirections ent maz) ((position.movement) ent) target

updateGhostPositions :: [Ghost] -> GameState -> [Ghost]
updateGhostPositions [] _      = []
updateGhostPositions xs gstate = [updateGhostPositions' x gstate | x <- xs]

--implement target tile pos algorithms here
updateGhostPositions' :: Ghost -> GameState -> Ghost
updateGhostPositions' gh@MkGhost{ghostName = Blinky} gstate = gh{targetTile = (position.movement.entity.player) gstate}
updateGhostPositions' gh@MkGhost{ghostName = Pinky} gstate = gh{targetTile = (getNextPos $ (position.movement.entity.player) gstate) ((direction.movement.entity.player) gstate) 4}
updateGhostPositions' gh@MkGhost{ghostName = Clyde} gstate = tilePositionClyde (player gstate) gh
updateGhostPositions' gh@MkGhost{ghostName = Inky} gstate = tilePositionInky (player gstate)  (getGhost (ghosts gstate) Blinky) gh
-- updateGhostPositions' g _      = g


-- Helper function to get Blinky's position
getGhost :: [Ghost] -> GhostType -> Ghost
getGhost ghosts gtype =
    case find (\g -> ghostName g == gtype) ghosts of
      Just blinky -> blinky
      Nothing     -> error "Ghost not found in the list of ghosts"

--not the most beautifull solution to get the correct tiledistance
-- not sure if this ACTUALLY the right algorithm for clyde but close enough ig
tilePositionClyde :: Player -> Ghost -> Ghost
tilePositionClyde player ghost | distanceTilePos playerPos ghostPos <= (4 * fst tileSize) =  ghost{targetTile = scatterCorner ghost}
                               | otherwise = ghost{targetTile = playerPos}
                                where playerPos =  (position.movement.entity) player
                                      ghostPos =  (position.movement.entityG) ghost


-- Inky's target tile logic
tilePositionInky :: Player -> Ghost -> Ghost -> Ghost
tilePositionInky player blinky inky =
    inky { targetTile =  (targetPosX, targetPosY)}
  where
    -- Calculate Pac-Man's position two tiles ahead
    playerPos = (position . movement . entity) player
    playerDir = (direction . movement . entity) player
    referenceTile = getNextPos playerPos playerDir 2

    -- Blinky's current position
    blinkyPos = (position . movement . entityG) blinky

    -- Calculate the vector from Blinky to the reference tile and double it
    (refX, refY) = referenceTile
    (blinkyX, blinkyY) = blinkyPos
    targetPosX = refX + 1.5 * (refX - blinkyX)
    targetPosY = refY + 1.5 * (refY - blinkyY)
    -- target = (bimap (targetPosX /) (targetPosY /) (tileSize))

--include a better function for handling a hit
checkGhosts :: GameState -> GameState
checkGhosts gstate@MkGameState{ghosts = xs, player = p} =
  case hitGhost of
    Just ghost -> handleGhostInteraction gstate ghost
    Nothing    -> gstate
  where
    -- Find the first ghost that matches the player's position
    hitGhost = find (\x -> snapToGrid (position . movement . entity $ p) == snapToGrid (position . movement . entityG $ x)) xs

-- Define conditions for Inky and Clyde to leave Home mode
inkyPelletRequirement, clydePelletRequirement :: GameState -> Int
inkyPelletRequirement _ = 30   -- Number of pellets eaten to release Inky
clydePelletRequirement MkGameState{pelletC} = fst pelletC `div` 3  -- Number of pellets eaten to release Clyde

--Main game loop
mainGameLoopGhosts :: GameState -> GameState
mainGameLoopGhosts gstate@MkGameState{ghosts, elapsedTime, pelletC} =
    gstate { ghosts = map updateGhostBehaviour ghosts }
  where
    updateGhostBehaviour ghost
      --toggle between Chase and Scatter based on elapsed time
      | extractTime (behaviourMode ghost) < elapsedTime && canLeaveHome ghost = toggleBehaviour ghost (behaviourMode ghost)

      -- Check if ghost can switch from Home to Scatter based on pellet requirement
      | isHome ghost && canLeaveHome ghost && (extractTime (behaviourMode ghost) < elapsedTime ) =
        ghost { behaviourMode = Scatter (elapsedTime + 7)}
      | otherwise = ghost

    -- Function to determine if Inky or Clyde can leave Home mode
    canLeaveHome ghost = case ghostName ghost of
      Inky  -> uncurry (-) pelletC >= inkyPelletRequirement gstate
      Clyde -> uncurry (-) pelletC   >= clydePelletRequirement gstate
      _     -> True  -- Blinky and Pinky can always leave Home

    -- Function to toggle between Chase and Scatter modes (Hate this function :) )
    toggleBehaviour ghost (Chase _)       = ghost { behaviourMode = Scatter (elapsedTime + 7)}
    toggleBehaviour ghost (Scatter _)     = ghost { behaviourMode = Chase (elapsedTime + 20)}
    toggleBehaviour ghost (Home _)        = ghost { behaviourMode = Scatter (elapsedTime + 7), entityG = (entityG ghost) { movement = (movement (entityG ghost)) { position = (15, -12) }}}
    toggleBehaviour ghost _               = ghost { behaviourMode = Chase (elapsedTime + 20)}


extractTime :: BehaviourMode -> Float
extractTime (Chase time)      = time
extractTime (Scatter time)    = time
extractTime (Frightened time) = time
extractTime (Home time)       = time



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
    resetPlayer = changeDirPlayer (p { entity = resetEntityPos (entity p) playerSpawnPos, lives = lives p -1}) Still





-- chech ghost for Frigtend ignoring the time paramter
isFrightened :: Ghost -> Bool
isFrightened ghost =
  case behaviourMode ghost of
  Frightened _ -> True
  _            -> False

--debug function for printing all ghosts (Names)
printActiveGhosts :: GameState -> IO ()
printActiveGhosts gstate =  putStrLn $ unwords (map (show . ghostName) (ghosts gstate))



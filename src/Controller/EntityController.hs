{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Controller.EntityController where

import qualified Data.Map       as Map

import           Model.Entities
import           Model.Maze
import           Model.Model

-- should add maze for collision detection?
moveStep :: Entity -> Float -> Entity
moveStep ent@MkEntity{movement} stepx =
  case direction movement of
    Model.Entities.Right -> ent{movement = movement{position = (x + stepx, fromIntegral @Int (round y))}}
    Model.Entities.Left  -> ent{movement = movement{position = (x - stepx, fromIntegral @Int (round y))}}
    Model.Entities.Up    -> ent{movement = movement{position = (fromIntegral @Int (round x), y + stepx)}}
    Model.Entities.Down  -> ent{movement = movement{position = (fromIntegral @Int (round x), y - stepx)}}
    Model.Entities.Still -> ent
 where
  (x, y) = position movement

-- implementation of tunneling prevention
moveWithCollision :: Entity -> Float -> Maze -> Entity
moveWithCollision ent totalMovement maze = helperFunction ent steps
 where
  stepSize = totalMovement / 3 -- around 100 collision checks
  steps = floor (totalMovement / stepSize) :: Int -- number of steps to check in between large step
  helperFunction entity 0 = entity -- base case
  helperFunction entity n =
    -- recursive helper function
    let moveEntity = moveStep entity stepSize -- move a small step
     in case checkEntCollision checkWall moveEntity 0.5 maze of -- if no collision do one more step, if collision return entity
          Nothing -> helperFunction moveEntity (n - 1)
          Just x  -> x

checkEntCollision ::
  (Entity -> Tile -> Maybe Entity) -> Entity -> Float -> Maze -> Maybe Entity
checkEntCollision f ent ran maze =
  case Map.lookup (getTilePos (x, y)) maze of
    Nothing   -> Nothing
    Just tile -> f ent tile
 where
  (x, y) = getNextPos ((position . movement) ent) ((direction . movement) ent) ran

checkWall :: Entity -> Tile -> Maybe Entity
checkWall ent (MkWall _) =
  Just $
    changeDirEnt
      ( ent
          { movement = (movement ent){position = snapToGrid ((position . movement) ent)}
          }
      )
      Still
checkWall _ _ = Nothing


changeDirEnt :: Entity -> Direction -> Entity
changeDirEnt ent dir = ent{movement = (movement ent){direction = dir}}

changeHeadingEnt :: Entity -> Direction -> Entity
changeHeadingEnt ent heading = ent{movement = (movement ent){heading = heading}}

-- could be reused to not change direction, but for the ghost to check next pos
-- implement a class with this function for the ghost and players
checkValidHeading :: Entity -> Float -> Maze -> Entity
checkValidHeading ent@MkEntity{movement = move} tol maze =
  let headingDir = (heading . movement) ent
      entity = changeDirEnt ent headingDir
   in case checkEntCollision checkWall entity 0.6 maze of
        Nothing -> if snapToGridApproxEqual (x, y) tol then changeDirEnt ent{movement = move{position = snapToGrid ((position.movement) ent)}} headingDir else ent
        Just _ -> ent
 where
  (x, y) = (position . movement) ent

-- if getNextPos is collision, keep direction make direction the one you want
snapToGridApproxEqual :: EntityPosition -> Float -> Bool
snapToGridApproxEqual (x1, y1) tol =
  abs (x1 - x2) <= tolerance && abs (y1 - y2) <= tolerance
 where
  (x2, y2) = snapToGrid (x1, y1)
  tolerance = tol -- 0.8% tilesize

changeDirPlayer :: Player -> Direction -> Player
changeDirPlayer player direction = player{entity = updateDirection}
 where
  updateDirection = changeDirEnt (entity player) direction

changeHeadPlayer :: Player -> Direction -> Player
changeHeadPlayer player direction = player{entity = updateHeading}
 where
  updateHeading = changeHeadingEnt (entity player) direction

snapToGrid :: EntityPosition -> EntityPosition
snapToGrid (x, y) = (fromIntegral @Int (round x), fromIntegral @Int (round y))

getNextPos :: EntityPosition -> Direction -> Float -> EntityPosition
getNextPos (x, y) dir ran = (x', y')
 where
  (x', y') = nPos dir
  nPos Model.Entities.Up    = (fromIntegral @Int (round x), y + ran)
  nPos Model.Entities.Left  = (x - ran, fromIntegral @Int (round y))
  nPos Model.Entities.Right = (x + ran, fromIntegral @Int (round y))
  nPos Model.Entities.Down  = (fromIntegral @Int (round x), y - ran)
  nPos _                    = (x, y)

getTilePos :: EntityPosition -> TilePosition
getTilePos (x, y) = (fromIntegral @Int (round x), fromIntegral @Int (round (-y)))


--consumables
checkConsumable :: GameState -> Player -> Maze -> GameState
checkConsumable state player maze =
  case Map.lookup (getTilePos (x, y)) maze of -- check inplace
    Nothing   -> state
    Just tile -> handleConsumable state player tile
 where
  (x, y) = (position . movement . entity) player

retrieveConsumable' :: Tile -> Maybe ConsumableType
retrieveConsumable' (MkFloor (MkConsumable cons)) = Just cons
retrieveConsumable' _                             = Nothing

handleConsumable :: GameState -> Player -> Tile -> GameState
handleConsumable state player tile =
  case retrieveConsumable' tile of
    Just cType -> handleConsumable' state tilePos cType
    _          -> state -- update score etc.
 where
  tilePos = getTilePos $ (position . movement . entity) player

--maybe this would benefit from some smaller helper functions
handleConsumable' :: GameState -> TilePosition -> ConsumableType -> GameState
handleConsumable' state@MkGameState{maze, player} pos cType =
  checkPelletCount (state
    { maze = Map.insert pos (MkFloor EmptyTile) maze
    , isNewMaze = True
    , player = updateScore cType player
    , pelletC = pelletC state -1
    , ghosts  = if cType == SuperPellet
                then reverseGhostDirections $ changeGhostBehaviour (ghosts state) (Frightened (elapsedTime state + 7))
                else ghosts state
    }) (pelletC state)

--checks if all pellets are eaten
checkPelletCount :: GameState -> Int -> GameState
checkPelletCount state 0 = state{status = toggleGameOver state}
checkPelletCount state _ = state

-- update with the correct values
updateScore :: ConsumableType -> Player -> Player
updateScore Pellet player      = player{score = score player + 10}
updateScore SuperPellet player = player{score = score player + 50}
updateScore Cherry player      = player{score = score player + 100}

cntPellets :: Maze -> Int
cntPellets = Map.foldr cntConsumables $ -1
    where
      cntConsumables (MkFloor (MkConsumable _)) acc = acc + 1
      cntConsumables _ acc                          = acc

toggleGameOver :: GameState -> GameStatus
toggleGameOver MkGameState{status = GameOver} = GameOver
toggleGameOver MkGameState{status = Running}  = GameOver
toggleGameOver MkGameState{status = x}        = x


--ghost player interaction
changeGhostBehaviour :: [Ghost] -> BehaviourMode -> [Ghost]
changeGhostBehaviour xs mode = [x{behaviourMode = mode} | x <-xs]

reverseGhostDirections :: [Ghost] -> [Ghost]
reverseGhostDirections [] = []
reverseGhostDirections ghosts =
  [ ghost{entityG = (entityG ghost){movement = (movement (entityG ghost)) { direction = oppositeDirection (direction (movement (entityG ghost))) }}} | ghost <- ghosts ]

checkPlayerDeath :: GameState -> Player -> GameState
checkPlayerDeath state MkPlayer{lives = 1} = state{status = toggleGameOver state} -- if not GameOver, could also pattern match
checkPlayerDeath state _ = state

oppositeDirection :: Direction -> Direction
oppositeDirection Model.Entities.Left  = Model.Entities.Right
oppositeDirection Model.Entities.Right = Model.Entities.Left
oppositeDirection Model.Entities.Up    = Model.Entities.Down
oppositeDirection Model.Entities.Down  = Model.Entities.Up
oppositeDirection Model.Entities.Still = Model.Entities.Still  -- Default for "Still"

-- Now define getOpDirection to handle entities and specific logic for "Still"
getOpDirection :: Entity -> Direction -> Direction
getOpDirection ent Model.Entities.Still = oppositeDirection (oldDirection ent)
getOpDirection _ dir                    = oppositeDirection dir

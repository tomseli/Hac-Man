{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Controller.EntityController where

import qualified Data.Map as Map
import Data.Sequence (Seq (Empty))
import Model.Entities
import Model.Maze
import Model.Model

-- should add maze for collision detection?
moveStep :: Entity -> Float -> Entity
moveStep ent@MkEntity{movement} stepx =
  case direction movement of
    Model.Entities.Right -> ent{movement = movement{position = (x + stepx, y)}}
    Model.Entities.Left -> ent{movement = movement{position = (x - stepx, y)}}
    Model.Entities.Up -> ent{movement = movement{position = (x, y + stepx)}}
    Model.Entities.Down -> ent{movement = movement{position = (x, y - stepx)}}
    Model.Entities.Still -> ent
 where
  (x, y) = position movement

-- implementation of tunneling prevention
moveWithCollision :: Entity -> Float -> Maze -> Entity
moveWithCollision ent totalMovement maze = helperFunction updatedEnt steps
 where
  stepSize = 0.1 -- around 100 collision checks
  steps = floor (totalMovement / stepSize) :: Int -- number of steps to check in between large step
  updatedEnt = checkValidHeading ent maze
  helperFunction entity 0 = entity -- base case
  helperFunction entity n =
    -- recursive helper function
    let moveEntity = moveStep entity stepSize -- move a small step
     in case checkEntCollision checkWall moveEntity 0.5 maze of -- if no collision do one more step, if collision return entity
          Nothing -> helperFunction moveEntity (n - 1)
          Just x -> x

checkEntCollision ::
  (Entity -> Tile -> Maybe Entity) -> Entity -> Float -> Maze -> Maybe Entity
checkEntCollision f ent ran maze =
  case Map.lookup (getTilePos (x, y)) maze of
    Nothing -> Nothing
    Just tile -> f ent tile
 where
  (x, y) = getNextPos ent ran

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

actOnCollision :: Entity -> Tile -> Maybe Entity
actOnCollision _ (MkFloor EmptyTile) = Nothing
actOnCollision _ (MkFloor (MkConsumable _)) = undefined
actOnCollision ent (MkWall _) =
  Just $
    changeDirEnt
      ( ent
          { movement = (movement ent){position = snapToGrid ((position . movement) ent)}
          }
      )
      Still

changeDirEnt :: Entity -> Direction -> Entity
changeDirEnt ent dir = ent{movement = (movement ent){direction = dir}}

changeHeadingEnt :: Entity -> Direction -> Entity
changeHeadingEnt ent heading = ent{movement = (movement ent){heading = heading}}

-- could be reused to not change direction, but for the ghost to check next pos
checkValidHeading :: Entity -> Maze -> Entity
checkValidHeading ent maze =
  let headingDir = (heading . movement) ent
      entity = changeDirEnt ent headingDir
   in case checkEntCollision checkWall entity 1 maze of
        Nothing -> if snapToGridApproxEqual (x, y) then changeDirEnt ent headingDir else ent
        Just _ -> ent
 where
  (x, y) = (position . movement) ent

-- if getNextPos is collision, keep direction make direction the one you want
snapToGridApproxEqual :: EntityPosition -> Bool
snapToGridApproxEqual (x1, y1) =
  abs (x1 - x2) <= tolerance && abs (y1 - y2) <= tolerance
 where
  (x2, y2) = snapToGrid (x1, y1)
  tolerance = 0.1 -- 10% tilesize

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

getNextPos :: Entity -> Float -> EntityPosition
getNextPos ent ran = (x', y')
 where
  dir = (direction . movement) ent
  (x, y) = (position . movement) ent
  (x', y') = nPos dir
  nPos Model.Entities.Up = (fromIntegral @Int (round x), y + ran)
  nPos Model.Entities.Left = (x - ran, fromIntegral @Int (round y))
  nPos Model.Entities.Right = (x + ran, fromIntegral @Int (round y))
  nPos Model.Entities.Down = (fromIntegral @Int (round x), y - ran)
  nPos _ = (x, y)

getTilePos :: EntityPosition -> TilePosition
getTilePos (x, y) = (fromIntegral @Int (round x), fromIntegral @Int (round (-y)))

testEntity :: Entity
testEntity =
  MkEntity
    { movement =
        MkMovement
          { direction = Still
          , speed = 8
          , position = (2, -2)
          , heading = Still
          }
    , alive = Alive
    }

testPlayer :: Player
testPlayer =
  MkPlayer
    { entity = testEntity
    , lives = 3
    , score = 0
    }

-- checkEntCollision :: (Entity -> Tile -> Maybe Entity) -> Entity -> Float -> Maze -> Maybe Entity
-- checkEntCollision f ent ran maze =
--   case Map.lookup (getTilePos (x, y)) maze of
--     Nothing -> Nothing
--     Just tile -> f ent tile
--  where
--   (x, y) = getNextPos ent ran

checkConsumable :: GameState -> Player -> Maze -> GameState
checkConsumable state player maze =
  case Map.lookup (getTilePos (x, y)) maze of -- check inplace
    Nothing -> state
    Just tile -> handleConsumable state player tile
 where
  (x, y) = (position . movement . entity) player

retrieveConsumable' :: Tile -> Maybe ConsumableType
retrieveConsumable' (MkFloor (MkConsumable cons)) = Just cons
retrieveConsumable' _ = Nothing

handleConsumable :: GameState -> Player -> Tile -> GameState
handleConsumable state player tile = case retrieveConsumable' tile of
  Just cType -> handleConsumable' state tilePos cType
  _ -> state -- update score etc.
 where
  tilePos = getTilePos $ (position . movement . entity) player

handleConsumable' :: GameState -> TilePosition -> ConsumableType -> GameState
handleConsumable' state@MkGameState{maze, player} pos cType =
  state
    { maze   = Map.insert pos (MkFloor EmptyTile) maze
    , player = updateScore cType player
    }

--update with the correct values
updateScore :: ConsumableType -> Player -> Player
updateScore Pellet player      = player{score = score player + 10}
updateScore SuperPellet player = player{score = score player + 50}
updateScore Cherry player      = player{score = score player + 100}
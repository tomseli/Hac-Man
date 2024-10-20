{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Controller.EntityController where

import qualified Data.Map as Map
import Model.Entities
import Model.Maze

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
     in case checkEntCollision moveEntity 0.5 maze of -- if no collision do one more step, if collision return entity
          Nothing -> helperFunction moveEntity (n - 1)
          Just x -> x

checkEntCollision :: Entity -> Float -> Maze -> Maybe Entity
checkEntCollision ent ran maze =
  case Map.lookup tilePos maze of
    Nothing -> Nothing
    Just tile -> actOnCollision ent tile
 where
  (x, y) = getNextPos ent ran
  tilePos :: TilePosition
  tilePos = (fromIntegral @Int (round x), fromIntegral @Int (round (-y)))

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

checkValidHeading :: Entity -> Maze -> Entity
checkValidHeading ent maze =
  let headingDir = (heading . movement) ent
      entity = changeDirEnt ent headingDir
   in case checkEntCollision entity 1 maze of
        Nothing -> changeDirEnt ent headingDir
        Just _ -> ent

-- if getNextPos is collision, keep direction make direction the one you want

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

testEntity :: Entity
testEntity =
  MkEntity
    { movement =
        MkMovement
          { direction = Still
          , speed = 11
          , position = (1, -1)
          , heading = Still
          }
    , alive = Alive
    }

testPlayer :: Player
testPlayer =
  MkPlayer
    { entity = testEntity
    , lives = 3
    }

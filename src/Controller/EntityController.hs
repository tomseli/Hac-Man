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
moveWithCollision ent totalMovement maze = helperFunction ent steps
 where
  stepSize = 0.01
  steps = floor (totalMovement / stepSize) :: Int -- number of steps to check in between large step
  helperFunction entity 0 = entity -- basecase
  helperFunction entity n =
    -- recursive helper function
    let moveEntity = moveStep entity stepSize -- move a small step
     in case checkEntCollision moveEntity maze of -- if no collision do one more step, if collision return x
          Nothing -> helperFunction moveEntity (n - 1)
          Just x -> x

interpolateRender :: Float -> Float -> Float
interpolateRender x1 x2 = x1 - x2

checkEntCollision :: Entity -> Maze -> Maybe Entity
checkEntCollision ent maze =
  case Map.lookup tilePos maze of
    Nothing -> Nothing
    Just tile -> actOnCollision ent tile
 where
  (x, y) = getNextPos ent -- (position.movement) ent
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

checkValidHeading :: Entity -> Entity
checkValidHeading = undefined

-- if getNextPos is collision, keep direction make direction the one you want

changeDirPlayer :: Player -> Direction -> Player
changeDirPlayer player direction = player{entity = updateDirection}
 where
  updateDirection = changeDirEnt (entity player) direction

snapToGrid :: EntityPosition -> EntityPosition
snapToGrid (x, y) = (fromIntegral @Int (round x), fromIntegral @Int (round y))

getNextPos :: Entity -> EntityPosition
getNextPos ent = (a, b)
 where
  (a, b) = snapToGrid (x', y')
  dir = (direction . movement) ent
  (x, y) = (position . movement) ent
  (x', y') = nPos dir
  nPos Model.Entities.Up = (x, y + 0.4)
  nPos Model.Entities.Left = (x - 0.4, y)
  nPos Model.Entities.Right = (x + 0.4, y)
  nPos Model.Entities.Down = (x, y - 0.4)
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

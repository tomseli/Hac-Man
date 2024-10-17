{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TypeApplications #-}

module Controller.EntityController where

import Model.Entities
import Model.Maze
import qualified Data.Map as Map

-- should add maze for collision detection?
moveStep :: Entity -> Float -> Entity
moveStep ent@MkEntity{movement} stepx =
  case direction movement of
    Model.Entities.Right -> ent{movement = movement{position =  (x + stepx, y)}}
    Model.Entities.Left  -> ent{movement = movement{position =  (x - stepx, y)}}
    Model.Entities.Up    -> ent{movement = movement{position =  (x, y + stepx)}}
    Model.Entities.Down  -> ent{movement = movement{position =  (x, y - stepx)}}
    Model.Entities.Still -> ent
 where
  (x, y) = position movement



interpolateRender :: Float -> Float -> Float
interpolateRender x1 x2 = x1 - x2

checkEntCollision :: Entity -> Maze -> Maybe Entity
checkEntCollision ent maze = 
  case Map.lookup tilePos maze of
    Nothing   -> Nothing
    Just tile -> actOnCollision ent tile
    where
      (x, y) = getNextPos ent
      tilePos :: TilePosition
      tilePos = (fromIntegral @Int (round x), fromIntegral @Int (round (-y)))

actOnCollision :: Entity -> Tile -> Maybe Entity
actOnCollision _ (MkFloor EmptyTile)        = Nothing
actOnCollision _   (MkFloor (MkConsumable _)) = undefined
actOnCollision ent (MkWall _)                 = Just $ changeDirEnt ent Still

changeDirEnt :: Entity -> Direction -> Entity
changeDirEnt ent dir = ent{movement = (movement ent){direction = dir}}

changeDirPlayer :: Player -> Direction -> Player
changeDirPlayer player direction = player{entity = updateDirection}
 where
  updateDirection = changeDirEnt (entity player) direction

snapToGrid :: EntityPosition -> EntityPosition
snapToGrid (x, y) = (fromIntegral @Int (round x), fromIntegral @Int (round y))

getNextPos ::  Entity -> EntityPosition
getNextPos ent = (a, b)
                        where
                          (a,b)       = snapToGrid (x', y')
                          dir        = (direction.movement) ent
                          (x , y)    = (position .movement) ent
                          (x', y')   = nPos dir
                          nPos Model.Entities.Up    = (x  ,y+0.4)
                          nPos Model.Entities.Left  = (x-0.4,  y)
                          nPos Model.Entities.Right = (x+0.4,  y)
                          nPos Model.Entities.Down  = (x  ,y-0.4)
                          nPos _                    = (x  , y)


testEntity :: Entity
testEntity =
  MkEntity
    { movement =
        MkMovement{direction = Model.Entities.Still, speed = 8, position = (1, -1)}
    , alive = Alive
    }

testPlayer :: Player
testPlayer =
  MkPlayer
    { entity = testEntity
    , lives = 3
    }

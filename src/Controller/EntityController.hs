{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Controller.EntityController where

import Model.Entities  

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

--should add maze for collision detection?
moveStep :: Entity -> Float -> Entity
moveStep ent@MkEntity{ movement } stepx = 
  case direction movement of
    Model.Entities.Right  -> ent {movement = movement {position = (x + stepx, y)}} 
    Model.Entities.Left   -> ent {movement = movement {position = (x - stepx, y)}} 
    Model.Entities.Up     -> ent {movement = movement {position = (x, y + stepx)}} 
    Model.Entities.Down   -> ent {movement = movement {position = (x, y - stepx)}} 
    Model.Entities.Still  -> ent
    where (x, y) = position movement

changeDirEnt :: Entity -> Direction -> Entity
changeDirEnt ent dir = ent {movement = (movement ent) {direction = dir}}

changeDirPlayer :: Player -> Direction -> Player
changeDirPlayer player direction = player {entity = updateDirection}
  where updateDirection = changeDirEnt (entity player) direction

collisionBorder :: Entity -> Entity
collisionBorder = undefined

testEntity :: Entity
testEntity = MkEntity 
                { 
                  movement = MkMovement{direction = Model.Entities.Still, speed = 1, position = (0, 0)},
                  alive    = Alive
                }

testPlayer :: Player
testPlayer = MkPlayer
              {
                entity = testEntity,
                lives  = 3
              }

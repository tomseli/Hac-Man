{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Controller.EntityController where

import qualified Data.Map         as Map

import           Model.CustomMaze
import           Model.Entities
import           Model.Maze
import           Model.Model

--move a single step in the direction of the entity
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
  stepSize = totalMovement / 3 -- around 3 collision checks
  steps = floor (totalMovement / stepSize) :: Int -- number of steps to check in between large step
  helperFunction entity 0 = entity -- base case
  helperFunction entity n =
    -- recursive helper function
    let moveEntity = moveStep entity stepSize -- move a small step
     in case checkEntCollision checkWall moveEntity 0.5 maze of -- if no collision do one more step, if collision return entity
          Nothing -> helperFunction moveEntity (n - 1)
          Just x  -> x

-- given a function, check for collision bases on the output of the function.
-- The Float is used to specify how many tile you want to look forward (1 being a single tile)
checkEntCollision ::
  (Entity -> Tile -> Maybe Entity) -> Entity -> Float -> Maze -> Maybe Entity
checkEntCollision f ent ran maze =
  case Map.lookup (getTilePos (x, y)) maze of
    Nothing   -> Nothing
    Just tile -> f ent tile
 where
  (x, y) = getNextPos ((position . movement) ent) ((direction . movement) ent) ran

--given an entity return if the entity is in a wall
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

--change direction of an entity
--Direction is for the direction PacMan is currently going
changeDirEnt :: Entity -> Direction -> Entity
changeDirEnt ent dir = ent{movement = (movement ent){direction = dir}}

--change the heading of an entity
--Heading is for intended direction!
changeHeadingEnt :: Entity -> Direction -> Entity
changeHeadingEnt ent heading = ent{movement = (movement ent){heading = heading}}

-- could be reused to not change direction, but for the ghost to check next pos
-- implement a class with this function for the ghost and players
checkValidHeading :: Entity -> Float -> Maze -> Entity
checkValidHeading ent@MkEntity{movement = move} tol maze =
  let headingDir = (heading . movement) ent
      entity = changeDirEnt ent headingDir
   in case checkEntCollision checkWall entity 0.6 maze of
        Nothing -> if snapToGridApproxEqual (x, y) tol 
                   then changeDirEnt ent{movement = move{position = snapToGrid ((position.movement) ent)}} headingDir 
                   else ent
        Just _ -> ent
 where
  (x, y) = (position . movement) ent

-- if getNextPos is collision, keep direction make direction the one you want
-- a snap to grid with a tolerance
snapToGridApproxEqual :: EntityPosition -> Float -> Bool
snapToGridApproxEqual (x1, y1) tol =
  abs (x1 - x2) <= tolerance && abs (y1 - y2) <= tolerance
 where
  (x2, y2) = snapToGrid (x1, y1)
  tolerance = tol -- 0.8% tilesize

--change direction of the player
--Direction is for the direction PacMan is currently going
changeDirPlayer :: Player -> Direction -> Player
changeDirPlayer player direction = player{entity = updateDirection}
 where
  updateDirection = changeDirEnt (entity player) direction

--change the heading of the player
--Heading is for intended direction!
changeHeadPlayer :: Player -> Direction -> Player
changeHeadPlayer player direction = player{entity = updateHeading}
 where
  updateHeading = changeHeadingEnt (entity player) direction

--returns the center of the closest tile to the given postion (NON converting)
snapToGrid :: EntityPosition -> EntityPosition
snapToGrid (x, y) = (fromIntegral @Int (round x), fromIntegral @Int (round y))

--get the next postion and round to stay in grid
getNextPos :: EntityPosition -> Direction -> Float -> EntityPosition
getNextPos (x, y) dir ran = (x', y')
 where
  (x', y') = nPos dir
  nPos Model.Entities.Up    = (fromIntegral @Int (round x), y + ran)
  nPos Model.Entities.Left  = (x - ran, fromIntegral @Int (round y))
  nPos Model.Entities.Right = (x + ran, fromIntegral @Int (round y))
  nPos Model.Entities.Down  = (fromIntegral @Int (round x), y - ran)
  nPos _                    = (x, y)

--conversion function
getTilePos :: EntityPosition -> TilePosition
getTilePos (x, y) = (fromIntegral @Int (round x), fromIntegral @Int (round (-y)))

--conversion function
fromTilePos :: TilePosition -> EntityPosition
fromTilePos (tileX, tileY) = (fromIntegral tileX, fromIntegral (-tileY))

--consumables
checkConsumable :: GameState -> Player -> Maze -> GameState
checkConsumable state player maze =
  case Map.lookup (getTilePos (x, y)) maze of -- check inplace
    Nothing   -> state
    Just tile -> handleConsumable state player tile
 where
  (x, y) = (position . movement . entity) player

--given a tile, give the ConsumableType
retrieveConsumable' :: Tile -> Maybe ConsumableType
retrieveConsumable' (MkFloor (MkConsumable cons)) = Just cons
retrieveConsumable' _                             = Nothing

handleConsumable :: GameState -> Player -> Tile -> GameState
handleConsumable state player tile =
  case retrieveConsumable' tile of
    Just cType -> handleConsumable' state tilePos cType -- update score etc.
    _          -> state
 where
  tilePos = getTilePos $ (position . movement . entity) player

--helper to check the pellet count and perform actions based on pellet eaten
handleConsumable' :: GameState -> TilePosition -> ConsumableType -> GameState
handleConsumable' state@MkGameState{maze, player} pos cType =
  checkPelletCount (state
    { maze = Map.insert pos (MkFloor EmptyTile) maze
    , isNewMaze = True
    , player = updateScore cType player
    , pelletC = (fst (pelletC state), snd (pelletC state) - 1) -- decrease pellet count, keeping the total
    , ghosts  = if cType == SuperPellet then updateFrightendState state (ghosts state)  else ghosts state -- frighten ghosts when superPellet is eaten
    }) (snd $ pelletC state)


--if allowed to be frightend, frighten. Otherwise stay in home
makeGhostFrightend :: GameState -> Ghost -> Ghost
makeGhostFrightend gstate g  | isHome g  = g
                             | otherwise = g{behaviourMode = Frightened  (elapsedTime gstate + 7)}

-- update all the ghosts that are allowed to be frightend, to frigthend
updateFrightendState :: GameState -> [Ghost] -> [Ghost]
updateFrightendState gstate xs = reverseGhostDirections [makeGhostFrightend gstate x | x <- xs]

--check if the ghosts are allowed to be frightend
isHome :: Ghost -> Bool
isHome MkGhost{behaviourMode = Home _} = True
isHome _                               = False

--resets level
resetLevel :: GameState -> GameState
resetLevel state = state{ghosts = nGhosts, maze = nmaze, player = nplayer, pelletC = pellets, level = level state + 1}
                  where
                    nplayer = (player state){entity = moveEntityPos ((entity.player) state) playerSpawnPos} --reset player, not score
                    nmaze = customMaze  --reset maze
                    nGhosts = map (\g -> g{entityG = (entityG g){movement = ((movement.entityG) g) -- change speed and reset to home positions
                                  {speed = (speed.movement.entityG) g + 1}}}) (resetGhost state (ghosts state))
                    pellets = (\(a,_) -> (a, a)) (pelletC state)

--resets all ghosts postion and behaviourmode to Home for x seconds
resetGhost :: GameState -> [Ghost] -> [Ghost]
resetGhost gstate xs = [resetGhost' gstate x | x <- xs]

--helpet to reset a single ghost
resetGhost' ::  GameState -> Ghost -> Ghost
resetGhost' gstate ghost@MkGhost{ entityG } =
   disableMovement ghost{ entityG = moveEntityPos entityG (homeTile ghost)  --teleport to hometile
                        , behaviourMode = Home (elapsedTime gstate + homeTime ghost)} -- stay in home for a set number of seconds after being eaten

--teleports entity position to a target tile
moveEntityPos :: Entity -> EntityPosition -> Entity
moveEntityPos ent@MkEntity{movement = move} (x, y) = ent{movement = move{position = (x, y)}}

--enable ghosts movement
enableMovement :: Ghost -> Ghost
enableMovement g = g{disAbleMove = False}

--disable ghosts movement
disableMovement :: Ghost -> Ghost
disableMovement g = g{disAbleMove = True}

--checks if all pellets are eaten
checkPelletCount :: GameState -> Int -> GameState
checkPelletCount state 0 = resetLevel state
checkPelletCount state _ = state

-- update with the correct values
updateScore :: ConsumableType -> Player -> Player
updateScore Pellet      player = player{ score = score player + 10 }
updateScore SuperPellet player = player{ score = score player + 50 }

-- Counts all the pellets in the Maze returning (Total, current)
cntPellets :: Maze -> (Int, Int)
cntPellets m = (nOfPellets, nOfPellets)
    where
      nOfPellets                                    = Map.foldr cntConsumables (-1) m
      cntConsumables (MkFloor (MkConsumable _)) acc = acc + 1
      cntConsumables _ acc                          = acc

--keeps the game in gameover
toggleGameOver :: GameState -> GameStatus
toggleGameOver MkGameState{status = GameOver} = GameOver
toggleGameOver MkGameState{status = Running}  = GameOver
toggleGameOver MkGameState{status = x}        = x

-- ghost player interaction
changeGhostBehaviour :: [Ghost] -> BehaviourMode -> [Ghost]
changeGhostBehaviour xs mode = [ x{ behaviourMode = mode } | x <-xs ]

-- reverses all ghosts
reverseGhostDirections :: [Ghost] -> [Ghost]
reverseGhostDirections [] = []
reverseGhostDirections ghosts =
  [ ghost{entityG = (entityG ghost)
   {movement = (movement (entityG ghost))
   { direction = oppositeDirection (direction (movement (entityG ghost))) }}}
   | ghost <- ghosts
  ] -- lenses would have been nice here :)

checkPlayerDeath :: GameState -> Player -> GameState
checkPlayerDeath state MkPlayer{lives = 1} = state{status = toggleGameOver state} -- if not GameOver, could also pattern match
checkPlayerDeath state _ = state

oppositeDirection :: Direction -> Direction
oppositeDirection Model.Entities.Left  = Model.Entities.Right
oppositeDirection Model.Entities.Right = Model.Entities.Left
oppositeDirection Model.Entities.Up    = Model.Entities.Down
oppositeDirection Model.Entities.Down  = Model.Entities.Up
oppositeDirection Model.Entities.Still = Model.Entities.Still  -- Default for "Still"

--gets a direction two steps in the past to prevent the ghosts from doing a U-turn in one tile...
getOpDirection :: Entity -> Direction -> Direction
getOpDirection ent Model.Entities.Still = oppositeDirection (oldDirection ent)
getOpDirection _ dir                    = oppositeDirection dir

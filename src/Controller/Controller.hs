{-# LANGUAGE NamedFieldPuns #-}
module Controller.Controller where

import           Controller.EntityController
import           Controller.GhostController

import qualified Graphics.Gloss.Interface.IO.Game as Gloss

import           Model.Entities
import           Model.Model

import Data.Char

import           System.Exit

import           View.RenderMaze
import           View.SaveHighScore

step :: Float -> GameState -> IO GameState
step dt state = do
  --save the next step so the Case statement below can choose between current and next state (for Pause and Gameover)
  let newState = checkGhosts $ mainGameLoopGhosts $ state{ elapsedTime = elapsedTime state + dt
                                                         , deltaTime   = dt
                                                         , player      = updatePlayer dt state
                                                         , ghosts      = updateGhosts dt state
                                                         }
  -- render optimization
  -- Updates the maze only if something in the maze has changed
  let updateMaze' = if isNewMaze newState
                    then newState{ isNewMaze = False
                                 , oldMaze = renderMaze newState Gloss.Blank}
                    else newState
  let updateMaze = checkConsumable updateMaze' (player state) (maze state)
  case status updateMaze of
    Running  -> return updateMaze
    Paused   -> return state
    Quitting -> exitSuccess
    GameOver ->  handleGameOver state


-- TODO: This function is difficult to expand
-- see if we can make this better
updatePlayer :: Float -> GameState -> Player
updatePlayer dt state =
  let
    updatedEntity = updatePlayerMovement dt state --update player movement
    finalEntity = case animation updatedEntity of
      (Just anim) -> updatedEntity{ animation = Just (updateAnimation anim state) }
      Nothing     -> updatedEntity
  in
    (player state) { entity = finalEntity }

-- retrieves next animation
updateAnimation :: Animation -> GameState -> Animation
updateAnimation anim s
  | elapsedTime s - lastUpdate anim > (1 / rate anim) = anim{ lastUpdate = elapsedTime s
                                                            , index = nextIndex
                                                            }
  | otherwise = anim
    where
      nextIndex = (index anim + 1) `mod` len
      len = length (frames anim)

-- Returns updated player entity (Player is retrieved from gameState since Maze is needed)
updatePlayerMovement :: Float -> GameState -> Entity
updatePlayerMovement dt state =
  moveWithCollision
    (checkValidHeading (entity (player state)) 0.08 (maze state)) --0.8% play in direction change
    ((speed . movement . entity . player) state * dt)
    (maze state)

--map the updateGhost function over all ghosts
updateGhosts :: Float -> GameState -> [Ghost]
updateGhosts dt state = [updateGhost dt state x | x <- updateGhostPositions (ghosts state) state]

-- update the position and animation of a ghost
-- this function takes a dt, the gamestate and the ghost to update
updateGhost :: Float -> GameState -> Ghost -> Ghost
updateGhost dt state ghost =
  let
    movedGhost = moveGhost state (enableMovementFromHome ghost) ((speed . movement . entityG) ghost * dt) (maze state)
    newAnimation = fmap (`updateAnimation` state) (animation movedGhost)
    updatedEntity = movedGhost { animation = newAnimation }
  in
    ghost {entityG = updatedEntity}

-- if in home, disable movement for home x seconds
enableMovementFromHome :: Ghost -> Ghost
enableMovementFromHome g = case behaviourMode g of
                              Home _ -> disableMovement g
                              _      -> enableMovement g

-- handles key and resize events
eventHandler :: Gloss.Event -> GameState -> IO GameState
eventHandler e state = return $ (handleKeys e . handleResize e) state

--resize handler
handleResize :: Gloss.Event -> GameState -> GameState
handleResize (Gloss.EventResize (x, y)) state = state{windowInfo = MkWindowInfo (x, y)}
handleResize _ state = state

--key handler
handleKeys :: Gloss.Event -> GameState -> GameState
handleKeys (Gloss.EventKey key keyState _ _) state
  | keyState == Gloss.Up = state
  | otherwise = case key of
      -- Esc to terminate the application
      (Gloss.SpecialKey Gloss.KeyEsc) -> state{status = Quitting}
      (Gloss.Char c) -> 
        case toLower c of 
          'a' -> state{player = changeHeadPlayer (player state) Model.Entities.Left}
          'w' -> state{player = changeHeadPlayer (player state) Model.Entities.Up}
          's' -> state{player = changeHeadPlayer (player state) Model.Entities.Down}
          'd' -> state{player = changeHeadPlayer (player state) Model.Entities.Right}
          'p' -> state{status = togglePause state}
          'o' -> toggleDebug state
          _ -> state
      _ -> state
handleKeys _ state = state

-- toggles the debug overlay
toggleDebug :: GameState -> GameState
toggleDebug state@MkGameState{enableDebug} = state{enableDebug = not enableDebug}

--toggles the paused overlay
togglePause :: GameState -> GameStatus
togglePause MkGameState{status = Paused}  = Running
togglePause MkGameState{status = Running} = Paused
togglePause _                             = Running

--handles gameover state
handleGameOver ::  GameState -> IO GameState
handleGameOver state = do
                      nState <- saveHighscore (updateHighscore (highscores state) ((score.player) state)) state -- write new highscores to file
                      return nState{status = GameOver}                                                          -- keep Type happy

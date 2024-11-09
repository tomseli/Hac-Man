{-# LANGUAGE NamedFieldPuns #-}
module Controller.Controller where

import           Controller.EntityController
import           Controller.GhostController

import qualified Graphics.Gloss.Interface.IO.Game as Gloss

import           Model.Entities
import           Model.Model

import           System.Exit
import View.RenderMaze

import           View.SaveHighScore

step :: Float -> GameState -> IO GameState
step dt state = do
  let newState = checkGhosts $ gotoScatterGhosts $ state{ elapsedTime = elapsedTime state + dt
                                                        , deltaTime   = dt
                                                        , player      = updatePlayer dt state
                                                        , ghosts      = updateGhosts dt state
                                                        }

  -- this line is in gross violation of the MVC pattern, look for alternatives
  -- stores an old maze render in the gamestate for optimization purposes
  let updateMaze' = if isNewMaze newState 
                    then newState{ isNewMaze = False
                                 , oldMaze = renderMaze newState Gloss.Blank} 
                    else newState 
  let updateMaze = checkConsumable updateMaze' (player state) (maze state)
  -- print $ show $ (lives.player) state
  -- print $ show $ status updateMaze
  -- print $ show $ behaviourMode (head $ ghosts state)
  -- print $ "elapsedTime = " ++  show ( elapsedTime state)
  -- print $ "unfrightenTime = " ++ show (unfrightenTime state)
  -- print $ "deltaTime = " ++ show (unfrightenTime state - elapsedTime state)
  case status updateMaze of
    Running  -> return updateMaze
    Paused   -> return state
    Quitting -> exitSuccess
    GameOver ->  handleGameOver state

handleGameOver ::  GameState -> IO GameState
handleGameOver state = do
                      nState <- saveHighscore (updateHighscore (highscores state) ((score.player) state)) state
                      return nState{status = GameOver}

-- TODO: This function is difficult to expand
-- see if we can make this better
updatePlayer :: Float -> GameState -> Player
updatePlayer dt state =
  let
    updatedEntity = updateMovement dt state
    finalEntity = case animation updatedEntity of
      (Just anim) -> updatedEntity{ animation = Just (updateAnimation anim state) }
      Nothing     -> updatedEntity
  in
    (player state) { entity = finalEntity }

updateAnimation :: Animation -> GameState -> Animation
updateAnimation anim s
  | elapsedTime s - lastUpdate anim > (1 / rate anim) = anim{ lastUpdate = elapsedTime s
                                                            , index = nextIndex
                                                            }
  | otherwise = anim
    where
      nextIndex = (index anim + 1) `mod` len
      len = length (frames anim)

updateMovement :: Float -> GameState -> Entity
updateMovement dt state =
  moveWithCollision
    (checkValidHeading (entity (player state)) 0.08 (maze state)) --0.15% play in direction change
    ((speed . movement . entity . player) state * dt)
    (maze state)


--  updatedEnt = checkValidHeading ent maze

updateGhosts :: Float -> GameState -> [Ghost]
updateGhosts dt state = [updateGhost dt state x | x <- updateGhostPositions (ghosts state) state]

-- same not as updatePlayer
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

eventHandler :: Gloss.Event -> GameState -> IO GameState
eventHandler e state = return $ (handleKeys e . handleResize e) state

handleResize :: Gloss.Event -> GameState -> GameState
handleResize (Gloss.EventResize (x, y)) state = state{windowInfo = MkWindowInfo (x, y)}
handleResize _ state = state

handleKeys :: Gloss.Event -> GameState -> GameState
handleKeys (Gloss.EventKey key keyState _ _) state
  | keyState == Gloss.Up = state
  | otherwise = case key of
      -- Esc to terminate the application
      (Gloss.SpecialKey Gloss.KeyEsc) -> state{status = Quitting}
      -- 'a' to turn player left
      (Gloss.Char 'a') ->
        state{player = changeHeadPlayer (player state) Model.Entities.Left}
      -- 'w' to turn player up
      (Gloss.Char 'w') ->
        state{player = changeHeadPlayer (player state) Model.Entities.Up}
      -- 's' to turn player Down
      (Gloss.Char 's') ->
        state{player = changeHeadPlayer (player state) Model.Entities.Down}
      -- 's' to turn player Right
      (Gloss.Char 'd') ->
        state{player = changeHeadPlayer (player state) Model.Entities.Right}
      -- 'p' pause the game
      (Gloss.Char 'p') ->
        state{status = togglePause state}
      -- 'q' purposely crashes the game
      (Gloss.Char 'q') ->
        error ":("
      -- o opens a debug overlay
      (Gloss.Char 'o') ->
        toggleDebug state
      _ -> state
handleKeys _ state = state

toggleDebug :: GameState -> GameState
toggleDebug state@MkGameState{enableDebug} = state{enableDebug = not enableDebug}

togglePause :: GameState -> GameStatus
togglePause MkGameState{status = Paused}  = Running
togglePause MkGameState{status = Running} = Paused
togglePause _                             = Running


--deprecated
-- checkStatus :: GameState -> IO GameState
-- checkStatus state@MkGameState{status} =
--   case status of
--     Quitting -> exitSuccess
--     Paused -> return state
--     _ -> return state

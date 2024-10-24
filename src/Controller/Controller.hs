{-# LANGUAGE NamedFieldPuns #-}

module Controller.Controller where
import Controller.EntityController
import Controller.GhostController
import qualified Graphics.Gloss.Interface.IO.Game as Gloss

import           Model.Entities
import           Model.Model

import           System.Exit

step :: Float -> GameState -> IO GameState
step dt state = do
  let newState = checkGhosts $ state{elapsedTime = elapsedTime state + dt, player = updatePlayer dt state, ghosts = updateGhosts dt state}
  let updateMaze = checkConsumable newState (player state) (maze state)
  case status updateMaze of
    Running  -> return updateMaze
    Paused   -> return state 
    Quitting -> exitSuccess
    _ -> return updateMaze

updatePlayer :: Float -> GameState -> Player
updatePlayer dt state =
  (player state)
    { entity =
        moveWithCollision
          (entity (player state))
          ((speed . movement . entity . player) state * dt)
          (maze state)
    }

updateGhosts :: Float -> GameState -> [Ghost]
updateGhosts dt state = [updateGhost dt state x | x <- updateGhostPositions (ghosts state) state] 

updateGhost :: Float -> GameState -> Ghost -> Ghost
updateGhost dt state ghost = ghost {entityG = moveGhost state ghost ((speed . movement . entityG) ghost * dt) (maze state)}

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
togglePause MkGameState{status = Paused} = Running
togglePause MkGameState{status = Running} = Paused
togglePause _ = Running


--deprecated
-- checkStatus :: GameState -> IO GameState
-- checkStatus state@MkGameState{status} =
--   case status of
--     Quitting -> exitSuccess
--     Paused -> return state
--     _ -> return state
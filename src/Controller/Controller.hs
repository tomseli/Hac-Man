{-# LANGUAGE NamedFieldPuns #-}

module Controller.Controller where

import Controller.EntityController
import qualified Graphics.Gloss.Interface.IO.Game as Gloss -- Event, EventKey
import Model.Entities
import Model.Model
import System.Exit

step :: Float -> GameState -> IO GameState
step dt state = do
  -- this should always be the last in the pipeline
  checkStatus
    state{elapsedTime = elapsedTime state + dt, player = updatedPlayer}
 where
  updatedPlayer =
    (player state)
      { entity =
          moveStep
            ((entity . player) state)
            ((speed . movement . entity . player) state * dt)
      }

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
        state{player = changeDirPlayer (player state) Model.Entities.Left}
      -- 'w' to turn player up
      (Gloss.Char 'w') -> state{player = changeDirPlayer (player state) Model.Entities.Up}
      -- 's' to turn player Down
      (Gloss.Char 's') ->
        state{player = changeDirPlayer (player state) Model.Entities.Down}
      -- 's' to turn player Right
      (Gloss.Char 'd') ->
        state{player = changeDirPlayer (player state) Model.Entities.Right}
      (Gloss.Char 'q') -> error ":("
      -- o opens a debug overlay
      (Gloss.Char 'o') -> toggleDebug state
      _ -> state
handleKeys _ state = state

toggleDebug :: GameState -> GameState
toggleDebug state@MkGameState{enableDebug} = state{enableDebug = not enableDebug}

checkStatus :: GameState -> IO GameState
checkStatus state@MkGameState{status} =
  case status of
    Quitting -> exitSuccess
    _ -> return state

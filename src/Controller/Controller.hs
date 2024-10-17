{-# LANGUAGE NamedFieldPuns #-}

module Controller.Controller where

import Controller.EntityController
    ( moveStep, checkEntCollision, changeDirPlayer )
import qualified Graphics.Gloss.Interface.IO.Game as Gloss -- Event, EventKey
import Model.Entities
    ( Direction(Right, Left, Up, Down),
      Entity(movement),
      Movement(speed),
      Player(entity) )

import Model.Model
    ( GameState(MkGameState, status, maze, elapsedTime, windowInfo,
                player, enableDebug),
      GameStatus(Quitting),
      WindowInfo(MkWindowInfo) )
import System.Exit (exitSuccess)

step :: Float -> GameState -> IO GameState
step dt state = do
  -- this should always be the last in the pipeline
  checkStatus
    state{elapsedTime = elapsedTime state + dt, player = updatePlayer dt state}


updatePlayer :: Float -> GameState -> Player
updatePlayer dt state =     (player state)
      {  entity = let moveEntity = moveStep ((entity . player) state) ((speed . movement . entity . player) state * dt) in
                  case checkEntCollision moveEntity (maze state) of
                      Nothing        -> moveEntity
                      Just _         -> (entity . player) state
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
      (Gloss.Char 'w') ->
        state{player = changeDirPlayer (player state) Model.Entities.Up}
      -- 's' to turn player Down
      (Gloss.Char 's') ->
        state{player = changeDirPlayer (player state) Model.Entities.Down}
      -- 's' to turn player Right
      (Gloss.Char 'd') ->
        state{player = changeDirPlayer (player state) Model.Entities.Right}
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

checkStatus :: GameState -> IO GameState
checkStatus state@MkGameState{status} =
  case status of
    Quitting -> exitSuccess
    _ -> return state

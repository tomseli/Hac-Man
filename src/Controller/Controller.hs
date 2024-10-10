{-# language NamedFieldPuns #-}

module Controller.Controller where 

import Model.Model 

import Graphics.Gloss.Interface.IO.Game -- Event, EventKey
import System.Exit


step :: Float -> GameState -> IO GameState
step secs state = do
  -- this should always be the last in the pipeline
  checkStatus state { elapsedTime = elapsedTime state + secs }

eventHandler :: Event -> GameState -> IO GameState
eventHandler e state = return $ (handleKeys e . handleResize e) state

handleResize :: Event -> GameState -> GameState
handleResize (EventResize (x, y)) state@GameState{windowInfo = WindowInfo _ s} = 
                                        state{windowInfo = WindowInfo (x, y) s}
handleResize _ state = state

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey key keyState _ _) state 
  | keyState == Up = state
  | otherwise = case key of
                  -- Esc to terminate the application
                  (SpecialKey KeyEsc)  -> state{status = Quitting}
                  -- 'a' to crash the application
                  (Char 'a')           -> undefined
                  -- o opens a debug overlay
                  (Char 'o')           -> toggleDebug state
                  _                    -> state 
handleKeys _ state = state

toggleDebug :: GameState -> GameState
toggleDebug state@GameState{enableDebug} = state{enableDebug = not enableDebug}

checkStatus :: GameState -> IO GameState
checkStatus state@GameState{status} = 
  case status of
    Quitting -> exitSuccess
    _        -> return state
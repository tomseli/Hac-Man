{-# language NamedFieldPuns #-}

module Controller where 

import Model 

import Graphics.Gloss.Interface.IO.Game -- Event, EventKey
import System.Exit


step :: Float -> GameState -> IO GameState
step secs state = do
  -- this should always be the last in the pipeline
  checkStatus state { elapsedTime = elapsedTime state + secs }


eventHandler :: Event -> GameState -> IO GameState
eventHandler e state = return $ handleKeys e state


handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey key keyState _ _) state 
  | keyState == Up = state
  | otherwise = case key of
                  (SpecialKey KeyEsc)  -> state{status = Quitting}
                  (Char 'a')           -> undefined
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

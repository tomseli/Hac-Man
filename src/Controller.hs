module Controller where 

import Model

step :: Float -> GameState -> IO GameState
step secs gstate = 
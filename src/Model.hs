module Model where 

data GameState = GameState {
                 elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState 0
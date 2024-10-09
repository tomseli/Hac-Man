module Model where 

data GameStatus = Running | GameOver | Paused | Quitting

data GameState = GameState {
                 status :: GameStatus,
                 elapsedTime :: Float,
                 position    :: (Float, Float),
                 enableDebug :: Bool
                 }

initialState :: GameState
initialState = GameState Running 0 (0, 0) False
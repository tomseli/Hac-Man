module Model.Model where 

data GameStatus = Running | GameOver | Paused | Quitting

data GameState = GameState 
                  { status :: GameStatus
                  , elapsedTime :: Float
                  , position    :: (Float, Float)
                  , enableDebug :: Bool
                  }


initialState :: GameState
initialState = GameState 
                { status = Running 
                , elapsedTime = 0 
                , position = (0, 0) 
                , enableDebug = False
                }
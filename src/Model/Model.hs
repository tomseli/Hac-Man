module Model.Model where 

data GameStatus = Running | GameOver | Paused | Quitting

data GameState = GameState 
                  { status           :: GameStatus
                  , elapsedTime      :: Float
                  , position         :: (Float, Float)
                  , enableDebug      :: Bool
                  , windowResolution :: (Int, Int)
                  }


initialState :: GameState
initialState = GameState 
                { status           = Running 
                , elapsedTime      = 0 
                , position         = (0, 0) 
                , enableDebug      = False
                , windowResolution = (0, 0) -- gets updated first frame
                }
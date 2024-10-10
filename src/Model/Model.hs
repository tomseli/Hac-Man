module Model.Model where 
import Graphics.Gloss.Data.Picture ( Point )  


data GameStatus = Running | GameOver | Paused | Quitting
data WindowInfo = WindowInfo { resolution :: (Int, Int)
                              , scaleImg     :: Float
                             }

type RelativePoint = (Int, Int)
type AbsolutePoint = (Int, Int)

data GameState = GameState 
                  { status          :: GameStatus
                  , elapsedTime     :: Float
                  , position        :: Point
                  , enableDebug     :: Bool
                  , windowInfo      :: WindowInfo
                  }


initialState :: GameState
initialState = GameState 
                { status           = Running 
                , elapsedTime      = 0 
                , position         = (0, 0) 
                , enableDebug      = True
                , windowInfo       = WindowInfo (0, 0) 0.8 -- gets updated first frame
                }
module Model.Model where 

import Model.Maze

import qualified Graphics.Gloss.Data.Picture as Gloss

data GameStatus = Running | GameOver | Paused | Quitting
newtype WindowInfo = MkWindowInfo {resolution :: (Int, Int)}

data GameState = MkGameState 
                  { status          :: GameStatus
                  , maze            :: Maze
                  , elapsedTime     :: Float
                  , position        :: Gloss.Point
                  , enableDebug     :: Bool
                  , windowInfo      :: WindowInfo
                  }

initialState :: GameState
initialState = MkGameState 
                { status           = Running 
                , elapsedTime      = 0 
                , position         = (0, 0) 
                , enableDebug      = True
                , windowInfo       = MkWindowInfo (0, 0)
                }
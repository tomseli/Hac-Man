module Model.Model where 

import qualified Data.Map as Map 
import Model.Entities  
import Controller.EntityController 

data GameStatus = Running | GameOver | Paused | Quitting
newtype WindowInfo = MkWindowInfo {resolution :: (Int, Int)}

data CornerOrientation = SE | NW | NE | SW
data WallOrientation = Horizontal | Vertical

data WallShape = MkCorner CornerOrientation
               | MkWallShape WallOrientation

data ConsumableType = Pellet | SuperPellet | Cherry

data FloorType = MkConsumable ConsumableType | EmptyTile
data Tile = MkFloor FloorType | MkWall WallShape

data GameState = MkGameState 
                  { status          :: GameStatus
                  , elapsedTime     :: Float
                  , enableDebug     :: Bool
                  , windowInfo      :: WindowInfo
                  , player          :: Player
                  }

type TilePosition = (Int , Int)
type Maze = Map.Map TilePosition Tile


initialState :: GameState
initialState = MkGameState 
                { status           = Running 
                , elapsedTime      = 0 
                , enableDebug      = True
                , windowInfo       = MkWindowInfo (0, 0)
                , player           = testPlayer
                }
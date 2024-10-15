module Model.Model where 
import Graphics.Gloss.Data.Picture ( Point )  

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
                  , position        :: Point
                  , enableDebug     :: Bool
                  , windowInfo      :: WindowInfo
                  }

type TilePosition = (Int , Int)
-- type Maze = Map TilePosition Tile


initialState :: GameState
initialState = MkGameState 
                { status           = Running 
                , elapsedTime      = 0 
                , position         = (0, 0) 
                , enableDebug      = True
                , windowInfo       = MkWindowInfo (0, 0)
                }
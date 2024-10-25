module Model.Entities where
data IsAlive = Alive | Dead

data Direction = Left | Right | Up | Down | Still deriving (Eq, Show)

data GhostType = Inky | Pinky | Blinky | Clyde

type EntityPosition = (Float, Float)

data Movement = MkMovement
  { direction :: Direction
  , speed     :: Float
  , position  :: EntityPosition
  , heading   :: Direction
  }

data BehaviourMode = Chase | Scatter | Frightened | Home deriving (Show)

type Lives = Int

data Entity = MkEntity
  { movement     :: Movement
  , oldDirection :: Direction
  , alive        :: IsAlive
  }

data Player = MkPlayer
  { entity :: Entity
  , lives  :: Lives
  , score  :: Int
  }

data Ghost = MkGhost
  { entityG       :: Entity
  , ghostName     :: GhostType
  , behaviourMode :: BehaviourMode
  , targetTile    :: EntityPosition
  , homeCorner    :: EntityPosition
  }

ghostEntity :: Entity
ghostEntity =
  MkEntity
    { movement =
        MkMovement
          { direction = Model.Entities.Left
          , speed = 5
          , position = (27, -2)
          , heading = Model.Entities.Left
          }
    , alive = Alive
    , oldDirection = Still
    }
initiateblinky :: Ghost
initiateblinky =
  MkGhost
    { entityG = ghostEntity
    , ghostName = Blinky
    , behaviourMode = Chase
    , homeCorner = (27, -2)
    , targetTile = (0, 0)
    }


pacmanEntity :: Entity
pacmanEntity =
  MkEntity
    { movement =
        MkMovement
          { direction = Still
          , speed = 6
          , position = (2, -2)
          , heading = Still
          }
    , alive = Alive
    , oldDirection = Still
    }

initiatePlayer :: Player
initiatePlayer =
  MkPlayer
    { entity = pacmanEntity
    , lives = 3
    , score = 0
    }

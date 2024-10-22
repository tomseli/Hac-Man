module Model.Entities where

data IsAlive = Alive | Dead

data Direction = Left | Right | Up | Down | Still deriving (Eq, Show)

data GhostType = Inky | Pinky | Blinky | Clyde

type EntityPosition = (Float, Float)

data Movement = MkMovement
  { direction :: Direction
  , speed :: Float
  , position :: EntityPosition
  , heading :: Direction
  }

data BehaviourMode = Chase | Scatter | Frightened | Home

type Lives = Int

data Entity = MkEntity
  { movement :: Movement
  , alive :: IsAlive
  }

data Player = MkPlayer
  { entity :: Entity
  , lives :: Lives
  , score :: Int
  }

data Ghost = MkGhost
  { entityG :: Entity
  , ghostName :: GhostType
  , behaviourMode :: BehaviourMode
  }



ghostEntity :: Entity
ghostEntity =
  MkEntity
    { movement =
        MkMovement
          { direction = Model.Entities.Left
          , speed = 5
          , position = (2, -2)
          , heading = Model.Entities.Left
          }
    , alive = Alive
    }
    
initiateblinky :: Ghost
initiateblinky =
  MkGhost
    { entityG = ghostEntity
    , ghostName = Blinky
    , behaviourMode = Chase
    }


pacmanEntity :: Entity
pacmanEntity =
  MkEntity
    { movement =
        MkMovement
          { direction = Still
          , speed = 8
          , position = (2, -2)
          , heading = Still
          }
    , alive = Alive
    }

initiatePlayer :: Player
initiatePlayer =
  MkPlayer
    { entity = pacmanEntity
    , lives = 3
    , score = 0
    }
{-# LANGUAGE InstanceSigs #-}
module Model.Entities where
data IsAlive = Alive | Dead

data Direction = Left | Right | Up | Down | Still deriving (Eq, Show)

data GhostType = Inky | Pinky | Blinky | Clyde deriving (Eq)

type EntityPosition = (Float, Float)

data Movement = MkMovement
  { direction :: Direction
  , speed     :: Float
  , position  :: EntityPosition
  , heading   :: Direction
  }

type Time = Float

data BehaviourMode = Chase Time | Scatter Time | Frightened Time| Home Time deriving (Show)

instance Eq BehaviourMode where
  (==) :: BehaviourMode -> BehaviourMode -> Bool
  Chase _ == Chase _           = True
  Scatter _ == Scatter _       = True
  Frightened _ == Frightened _ = True
  Home _  == Home _            = True
  _ == _                       = False

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
  , homeTile      :: EntityPosition
  , scatterCorner :: EntityPosition
  , disAbleMove   :: Bool
  }

instance Eq Ghost where
  (==) :: Ghost -> Ghost -> Bool
  g1 == g2 = ghostName g1 == ghostName g2

ghostEntity :: Entity
ghostEntity =
  MkEntity
    { movement =
        MkMovement
          { direction =  Model.Entities.Still
          , speed = 5
          , position = (27, -2)
          , heading = Model.Entities.Still
          }
    , alive = Alive
    , oldDirection = Still
    }
initiateblinky :: Ghost
initiateblinky =
  MkGhost
    { entityG = ghostEntity
    , ghostName = Blinky
    , behaviourMode = Home 7 --start frightend for seven seconds
    , homeTile = (0, 0)
    , scatterCorner = (27, -2)
    , targetTile = (0, 0)
    , disAbleMove = True
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

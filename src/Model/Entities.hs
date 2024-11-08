{-# LANGUAGE InstanceSigs #-}
module Model.Entities where

import qualified Graphics.Gloss as Gloss

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

data Animation = MkAnimation
  { frames     :: [Gloss.Picture]
  , index      :: Int
  , rate       :: Float
  , lastUpdate :: Float
  }

data Entity = MkEntity
  { movement     :: Movement
  , oldDirection :: Direction
  , alive        :: IsAlive
  , animation    :: Maybe Animation
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

ghostEntity :: EntityPosition -> Entity
ghostEntity pos =
  MkEntity
    { movement =
        MkMovement
          { direction =  Model.Entities.Still
          , speed = 3
          , position = pos
          , heading = Model.Entities.Still
          }
    , animation = Nothing
    , alive = Alive
    , oldDirection = Still
    }

initiateblinky :: Ghost
initiateblinky =
  MkGhost
    { entityG = ghostEntity (15, -12) --spawnposition
    , ghostName = Blinky
    , behaviourMode = Home 0 --start in home for 0 seconds
    , homeTile = (15, -12) -- position after reset (eaten)
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
          , animation = Nothing -- animations are added later
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

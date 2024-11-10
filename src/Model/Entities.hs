{-# LANGUAGE InstanceSigs #-}
module Model.Entities where

import qualified Graphics.Gloss as Gloss

data Direction = Left | Right | Up | Down | Still deriving (Eq, Show)

data GhostType = Inky | Pinky | Blinky | Clyde deriving (Eq, Show)

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

-- Entity record that has all common types for each entity (ghost or player)
data Entity = MkEntity
  { movement     :: Movement --holds all data regarding the movement
  , oldDirection :: Direction --record of the old direction used to differntiate between heading and direction
  , animation    :: Maybe Animation
  }

--player record
data Player = MkPlayer
  { entity :: Entity
  , lives  :: Lives
  , score  :: Int
  }

--ghost record
data Ghost = MkGhost
  { entityG       :: Entity         --all movement etc.
  , ghostName     :: GhostType      --name of the ghost (used for patternmatch and debug)
  , behaviourMode :: BehaviourMode  --current behaviour mode
  , targetTile    :: EntityPosition --current target Tile
  , homeTile      :: EntityPosition --respawn position
  , scatterCorner :: EntityPosition --targetTile when behaviour is scatter
  , disAbleMove   :: Bool           --disables the ghost
  , homeTime      :: Float -- number of seconds the ghost stays in home after being eaten
  }

instance Eq Ghost where
  (==) :: Ghost -> Ghost -> Bool
  g1 == g2 = ghostName g1 == ghostName g2

--a single template for all ghosts
ghostEntity :: EntityPosition -> Entity
ghostEntity pos =
  MkEntity
    { movement =
        MkMovement
          { direction =  Model.Entities.Still
          , speed = 4
          , position = pos
          , heading = Model.Entities.Still
          }
    , animation = Nothing
    , oldDirection = Still
    }


-- below are all the initializations for each ghost
initiateblinky :: Ghost
initiateblinky =
  MkGhost
    { entityG = ghostEntity (15, -12) --spawnposition
    , ghostName = Blinky
    , behaviourMode = Home 0 --start in home for 0 seconds
    , homeTile =  (15, -12) -- position after reset (eaten)
    , scatterCorner = (27, -2)
    , targetTile = (0, 0)
    , disAbleMove = True
    , homeTime = 1
    }

initiatePinky :: Ghost
initiatePinky =
  MkGhost
    { entityG = ghostEntity (15, -15) --spawnposition
    , ghostName = Pinky
    , behaviourMode = Home 0 --start in home for 0 seconds
    , homeTile = (15, -15) -- position after reset (eaten)
    , scatterCorner = (2, -2)
    , targetTile = (0, 0)
    , disAbleMove = True
    , homeTime = 1
    }


initiateClyde :: Ghost
initiateClyde =
  MkGhost
    { entityG = ghostEntity (16, -15) --spawnposition
    , ghostName = Clyde
    , behaviourMode = Home 0 --start in home for 0 seconds
    , homeTile =  (16, -15) -- position after reset (eaten)
    , scatterCorner = (2, -30)
    , targetTile = (0, 0)
    , disAbleMove = True
    , homeTime = 7
    }

initiateInky :: Ghost
initiateInky =
  MkGhost
    { entityG = ghostEntity (14, -15) --spawnposition
    , ghostName = Inky
    , behaviourMode = Home 0 --start in home for 0 seconds
    , homeTile = (14, -15) -- position after reset (eaten)
    , scatterCorner = (27, -30)
    , targetTile = (0, 0)
    , disAbleMove = True
    , homeTime = 7
    }


pacmanEntity :: Entity
pacmanEntity =
  MkEntity
    { movement =
        MkMovement
          { direction = Still
          , speed = 6
          , position = (14, -24)
          , heading = Still
          }
          , animation = Nothing -- animations are added later
    , oldDirection = Still
    }

initiatePlayer :: Player
initiatePlayer =
  MkPlayer
    { entity = pacmanEntity
    , lives = 3
    , score = 0
    }

playerSpawnPos :: EntityPosition
playerSpawnPos = (14, -24)

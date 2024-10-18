module Model.Entities where

data IsAlive = Alive | Dead

data Direction = Left | Right | Up | Down | Still

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
  }

data GhostType = Inky | Pinky | Blinky | Clyde

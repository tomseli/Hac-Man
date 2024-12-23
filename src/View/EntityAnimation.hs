{-# LANGUAGE NamedFieldPuns #-}
module View.EntityAnimation where

import           Data.Maybe

import qualified Graphics.Gloss       as Gloss
import qualified Graphics.Gloss.Juicy as Juicy

import           Model.Entities
import           Model.Maze
import           Model.Model

import           View.Transform


loadPlayerAnimation :: IO [Gloss.Picture]
loadPlayerAnimation = loadAnimation "assets\\pacman\\frame" 10

loadBlinkyAnimation :: IO [Gloss.Picture]
loadBlinkyAnimation = loadAnimation "assets\\blinky\\frame" 2

loadPinkyAnimation :: IO [Gloss.Picture]
loadPinkyAnimation = loadAnimation "assets\\pinky\\frame" 2

loadInkyAnimation :: IO [Gloss.Picture]
loadInkyAnimation = loadAnimation "assets\\inky\\frame" 2

loadClydeAnimation :: IO [Gloss.Picture]
loadClydeAnimation = loadAnimation "assets\\clyde\\frame" 2

loadAnimation :: FilePath -> Int -> IO [Gloss.Picture]
loadAnimation path n = mapM f (getAnimPathsPNG path n)
  where
    f x = do
      pic <- Juicy.loadJuicyPNG x
      return (fromJust pic)

-- "assets\pacman\frame" -> 5 -> ["assets\pacman\frame0.png", ..., "assets\pacman\frame4.png"]
getAnimPathsPNG :: String -> Int -> [FilePath]
getAnimPathsPNG path n =
  [s ++ show x ++ ".png" | s <- [path], x <- take n [0..] :: [Int]]

storePlayerAnimation :: [Gloss.Picture] -> GameState -> GameState
storePlayerAnimation newAnimation s = s
  { player = (player s)
    { entity = (entity (player s))
      { animation = Just (MkAnimation
        { frames = newAnimation
        , index = 0
        , rate = 30
        , lastUpdate = 0
        })
      }
    }
  }

storeGhostAnimation :: [[Gloss.Picture]] -> GameState -> GameState
storeGhostAnimation newAnimations s =
  s { ghosts = zipWith updateGhostAnimation (ghosts s) newAnimations }
  where
    updateGhostAnimation ghost frames = ghost
      { entityG = (entityG ghost)
        { animation = Just (MkAnimation
          { frames = frames
          , index = 0
          , rate = 5
          , lastUpdate = 0
          })
        }
      }

renderPlayerAnimation :: Player -> Maze -> Gloss.Picture -> Gloss.Picture
renderPlayerAnimation player@MkPlayer{entity} maze pic =
  pic <> (transformToMaze maze . transformToPlayer player) (renderAnimation (animation entity))

-- inverts the image when the ghost is frightened 
renderGhostAnimation :: Ghost -> Maze -> Gloss.Picture -> Gloss.Picture
renderGhostAnimation ghost@MkGhost{entityG, behaviourMode = Frightened _} maze pic =
  pic <> (transformToMaze maze . transformToGhost ghost) (Gloss.scale 1 (-1) $ renderAnimation (animation entityG))
renderGhostAnimation ghost@MkGhost{entityG} maze pic =
  pic <> (transformToMaze maze . transformToGhost ghost) (renderAnimation (animation entityG))

-- animation is rendered here, index is incremented in the controller "updateAnimation"
renderAnimation :: Maybe Animation -> Gloss.Picture
renderAnimation (Just MkAnimation{frames=xs, index=idx}) = xs !! idx
renderAnimation Nothing                                  = error "Missing animation"

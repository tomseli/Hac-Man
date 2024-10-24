module View.EntityAnimation where

import           Data.Maybe

import qualified Graphics.Gloss       as Gloss
import qualified Graphics.Gloss.Juicy as Juicy

import           Model.Entities
import           Model.Model

loadPlayerAnimation :: IO [Gloss.Picture]
loadPlayerAnimation = mapM f (getAnimPathsPNG "assets\\pacman\\frame" 10)
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
          { animation = Just newAnimation }
      }
  }

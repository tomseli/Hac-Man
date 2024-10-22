module View.Transform where

import qualified Data.Map as Map
import Data.Maybe
import qualified Graphics.Gloss.Data.Picture as Gloss
import Model.Maze
import Model.Model

gameArea :: (Int, Int)
gameArea = (820, 1024)

tileSize :: (Float, Float)
tileSize = (25, 25)

-- centers the picture in the window
-- scales the picture if window < gameArea
transformPicture :: WindowInfo -> Gloss.Picture -> Gloss.Picture
transformPicture MkWindowInfo{resolution = (x, y)} pic = do
  -- find offset the picture to the top left
  let xOffset = fst gameArea `div` 2
  let yOffset = snd gameArea `div` 2

  -- find which scale is needed to make the window fit
  let sx = fromIntegral x / fromIntegral (fst gameArea)
  let sy = fromIntegral y / fromIntegral (snd gameArea)
  -- pick the lowest, maximum of 1
  let fScale = clamp 0 1 $ min sx sy
  Gloss.scale fScale fScale $
    Gloss.translate (fromIntegral (-xOffset)) (fromIntegral yOffset) pic

clamp :: Ord a => a -> a -> a -> a
clamp mn mx = max mn . min mx

transformToMaze :: Maze -> Gloss.Picture -> Gloss.Picture
transformToMaze m = Gloss.translate x (-y)
 where
  (x, y) = calculateMazeOffset m

calculateMazeOffset :: Maze -> (Float, Float)
calculateMazeOffset m = (xOffset, yOffset)
 where
  -- assumption: maze is not empty
  -- assumption: the maximum key is the same as the shape of the maze - 1
  ((mazeX, mazeY), _) = fromJust $ Map.lookupMax m
  (mX, mY) = (mazeX + 1, mazeY + 1)
  winXOffset = fst gameArea `div` 2
  winYOffset = snd gameArea `div` 2
  mXOffset = (mX * round (fst tileSize)) `div` 2
  mYOffset = (mY * round (snd tileSize)) `div` 2
  xOffset = fromIntegral $ winXOffset - mXOffset
  yOffset = fromIntegral $ winYOffset - mYOffset
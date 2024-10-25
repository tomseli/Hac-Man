{-# LANGUAGE TypeApplications #-}
module View.Transform where

import qualified Data.Map                    as Map
import           Data.Maybe

import qualified Graphics.Gloss.Data.Picture as Gloss
import qualified Graphics.Gloss.Data.Color   as Gloss

import           Model.Entities
import           Model.Maze
import           Model.Model

import           Prelude                     hiding (Left, Right)

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

transformToGhost :: Ghost -> Gloss.Picture -> Gloss.Picture
transformToGhost g = transformToEntity (entityG g) . rotateToGhost g

transformToPlayer :: Player -> Gloss.Picture -> Gloss.Picture
transformToPlayer p = transformToEntity (entity p) . rotateToPlayer p

transformToEntity :: Entity -> Gloss.Picture -> Gloss.Picture
transformToEntity 
  MkEntity{ 
    movement = MkMovement{ 
      direction = dir, position = (x, y)}} = 
    Gloss.translate
      (x' * fst tileSize + fst tileSize / 2)
      (y' * snd tileSize - snd tileSize / 2)
    where
      (x', y') = case dir of
        Right -> (x, fromIntegral @Int (round y))
        Left  -> (x, fromIntegral @Int (round y))
        Up    -> (fromIntegral @Int (round x), y)
        Down  -> (fromIntegral @Int (round x), y)
        _     -> (x, y)

rotateToPlayer :: Player -> Gloss.Picture -> Gloss.Picture
-- in case of still
rotateToPlayer MkPlayer{ 
    entity = MkEntity{ 
      movement = MkMovement{ 
        direction = Still}}} _ = 
          Gloss.color Gloss.yellow (Gloss.ThickCircle 0 25)
-- in case of movement
rotateToPlayer MkPlayer{ 
    entity = MkEntity{ 
      movement = MkMovement{ 
        direction = dir}}} pic = 
          Gloss.rotate deg pic 
          where 
            deg = case dir of
              Right -> 0
              Down  -> 90
              Left  -> 180
              Up    -> 270

-- for now there's only two states, left or right
-- no animations available for up and down
rotateToGhost :: Ghost -> Gloss.Picture -> Gloss.Picture
rotateToGhost 
  MkGhost{
    entityG = MkEntity{
      movement = MkMovement{
        direction = dir}}} = Gloss.scale x 1
          where 
            x = case dir of
                  Right -> -1
                  Down  -> -1
                  _     ->  1

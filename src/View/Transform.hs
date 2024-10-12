module View.Transform where

import Graphics.Gloss.Data.Picture ( scale, translate, Picture )

import Model.Model ( WindowInfo(..) )

gameArea :: (Int, Int)
gameArea = (820, 1024)

-- centers the picture in the window
-- scales the picture if window < gameArea
transformPicture:: WindowInfo -> Picture -> Picture 
transformPicture WindowInfo{resolution = (x, y)} pic = do
  -- find offset the picture to the top left 
  let xOffset = fst gameArea `div` 2
  let yOffset = snd gameArea `div` 2

  -- find which scale is needed to make the window fit 
  let sx = fromIntegral x / fromIntegral (fst gameArea)
  let sy = fromIntegral y / fromIntegral (snd gameArea)
  -- pick the lowest, maximum of 1
  let fScale = clamp 0 1 $ min sx sy
  scale fScale fScale $ translate (fromIntegral (-xOffset)) (fromIntegral yOffset) pic

clamp :: Ord a => a -> a -> a -> a
clamp mn mx = max mn . min mx 
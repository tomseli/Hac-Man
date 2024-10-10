module View.Transform where

import Graphics.Gloss.Data.Picture

import Model.Model


transformPicture:: WindowInfo -> Picture -> Picture --state@GameState{windowInfo = WindowInfo _ s}
transformPicture WindowInfo{resolution = (x, y), scaleImg = s} pic = do
  let xOffset = 1280 `div` 2
  let yOffset = 720 `div` 2
  let sx =  fromIntegral x / 1280
  let sy = fromIntegral y / 720
  let fScale = clamp 0 1 $ min sx sy
  scale fScale fScale $ translate (fromIntegral (-xOffset)) (fromIntegral yOffset) pic

clamp :: Ord a => a -> a -> a -> a
clamp mn mx = max mn . min mx 
-- getRelativePos :: GameState -> AbsolutePoint -> RelativePoint
-- getRelativePos state{windowResolution = (x, y)} p = undefined
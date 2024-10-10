module View.Transform where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Environment

import Model.Model


transformPicture:: GameState -> Picture -> Picture
transformPicture GameState{windowResolution = (x, y)} pic = do 
  let xOffset = x `div` 2
  let yOffset = y `div` 2
  translate (fromIntegral (-xOffset)) (fromIntegral yOffset) pic

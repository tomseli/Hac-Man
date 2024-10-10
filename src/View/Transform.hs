module View.Transform where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Environment


transformPicture:: Picture -> IO Picture
transformPicture pic = do 
  (x, y) <- getScreenSize
  let xOffset = x `div` 2
  let yOffset = y `div` 2
  return $ translate (fromIntegral (-xOffset)) (fromIntegral (yOffset)) pic

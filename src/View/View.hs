module View.View where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

import Model.Model
import View.Transform (transformPicture)

 -- picture pipeline, add functions with signature func:: Picture -> Picture
render :: GameState -> IO Picture
render state = do
  return $ transformPicture state $  (renderDebugInfo state . renderLogo) Blank 

renderLogo:: Picture -> Picture
renderLogo pic = 
  (color white . translate 700 (-125)) (color yellow (text "PACMAN")) <> pic

renderDebugInfo :: GameState -> Picture -> Picture
renderDebugInfo state@GameState{enableDebug = debug} pic 
  | debug     = renderBoundingBox <> renderDebugTimer state <> pic
  | otherwise = Blank <> pic

renderDebugTimer :: GameState -> Picture
renderDebugTimer GameState{elapsedTime = time} = 
  (color red . translate 10 (-25) . scale 0.25 0.25) $ text (show time)

renderBoundingBox :: Picture
renderBoundingBox = 
  color (makeColor 0 1 0 0.15) $ 
  polygon [(0,0), (1920, 0), (1920, -1080), (0, -1080)] 
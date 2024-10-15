module View.View where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

import Model.Model
import View.Transform ( transformPicture, gameArea )

 -- picture pipeline, add functions with signature func:: Picture -> Picture
render :: GameState -> IO Picture
render state@MkGameState{windowInfo = wInfo} = do
  return $ transformPicture wInfo $  (renderDebugInfo state . renderLogo) Blank 

renderLogo:: Picture -> Picture
renderLogo pic = 
  (color white . translate 150 (-125)) (color yellow (text "PACMAN")) <> pic

renderDebugInfo :: GameState -> Picture -> Picture
renderDebugInfo state@MkGameState{enableDebug = debug} pic 
  | debug     = renderGameArea <> renderDebugTimer state <> pic
  | otherwise = Blank <> pic

renderDebugTimer :: GameState -> Picture
renderDebugTimer MkGameState{elapsedTime = time} = 
  (color red . translate 10 (-25) . scale 0.25 0.25) $ text (show time)

renderGameArea :: Picture
renderGameArea = 
  color (makeColor 0 1 0 0.15) $ 
  polygon [(0,0), (x, 0), (x, -y), (0, -y)]
  where
    x = fromIntegral $ fst gameArea
    y = fromIntegral $ snd gameArea 
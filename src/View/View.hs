module View.View where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

import Model.Model
import View.Transform

 -- picture pipeline, add functions with signature func:: Picture -> Picture
render :: GameState -> IO Picture
render state= do
  return $  (debugInfo state 
            . renderLogo) Blank 

renderLogo:: Picture -> Picture
renderLogo pic = 
  (color white . transformTranslate (-250) 350) (color yellow (text "PACMAN")) <> pic

debugInfo :: GameState -> Picture -> Picture
debugInfo state@GameState{enableDebug = debug} pic | debug     = renderDebugTimer state <> pic
                                                   | otherwise = Blank <> pic

renderDebugTimer :: GameState -> Picture
renderDebugTimer GameState{elapsedTime = time} = 
  (color red . transformTranslate (-750) 450 . scale 0.25 0.25) $ text (show time)
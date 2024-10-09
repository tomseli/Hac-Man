module View where

import Model
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color


render :: GameState -> IO Picture
render state= do
    return $ (debugInfo state . dummyFunc state) Blank 


dummyFunc:: GameState -> Picture -> Picture
dummyFunc GameState{position = (x, y)} pic = (color white . translate x y) (color yellow (text "PACMAN")) <> pic


renderDebugInfo :: String -> Picture
renderDebugInfo s = (color red . translate (-100) (-100)) (text s)


debugInfo :: GameState -> Picture -> Picture
-- renderDebugInfo
debugInfo GameState{enableDebug = debug, elapsedTime = time} pic | debug = renderDebugInfo (show time) <> pic
                                                                 | otherwise = Blank <> pic
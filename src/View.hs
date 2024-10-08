module View where 

import Model 
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color


render :: GameState -> IO Picture
render GameState{position = (x, y)} = return $ ((color white) . translate x y) (circle 50)
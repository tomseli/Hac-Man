module View.Transform where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Environment

transformTranslate :: Float -> Float -> Picture -> Picture
transformTranslate x y pic = translate x y pic


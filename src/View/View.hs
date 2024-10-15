{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module View.View where

import Model.Model
import View.Transform ( transformPicture, gameArea )
import Model.Entities

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

tileSize :: (Float, Float)
tileSize = (32, 32)

 -- picture pipeline, add functions with signature func:: Picture -> Picture
render :: GameState -> IO Picture
render state@MkGameState{windowInfo = wInfo, player = wPlayer} = do
  return $ transformPicture wInfo $  (renderDebugInfo state . renderLogo . renderPlayer wPlayer) Blank

-- Add a bitmap (Add, when making the renderEntity)
renderPlayer :: Player -> Picture -> Picture
renderPlayer MkPlayer { entity } = renderEntity entity

renderEntity :: Entity -> Picture -> Picture
renderEntity MkEntity{ movement } pic =
  translate (newX * fst tileSize) (newY * snd tileSize) $ color yellow (ThickCircle 0 30) <> pic
    where (x, y) = position movement
          newX   = fromIntegral @Int (round x)
          newY   = fromIntegral @Int (round y)


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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
module View.View where

import qualified Graphics.Gloss.Data.Picture as Gloss
import qualified Graphics.Gloss.Data.Color as Gloss

import Model.Model
import Model.Maze
import View.Transform ( transformPicture, gameArea )
import qualified Data.Map as Map

tileSize :: (Float, Float)
tileSize = (16, 16)

 -- picture pipeline, add functions with signature func:: Picture -> Picture
render :: GameState -> IO Gloss.Picture
render state@MkGameState{windowInfo = wInfo} = do
  return $ transformPicture wInfo $  (renderDebugInfo state 
                                     . renderLogo
                                     . renderMaze state) Gloss.Blank 

renderLogo:: Gloss.Picture -> Gloss.Picture
renderLogo pic = 
  (Gloss.color Gloss.white . Gloss.translate 150 (-125)) 
  (Gloss.color Gloss.yellow (Gloss.text "PACMAN")) <> pic

renderDebugInfo :: GameState -> Gloss.Picture -> Gloss.Picture
renderDebugInfo state@MkGameState{enableDebug = debug} pic 
  | debug     = renderGameArea <> renderDebugTimer state <> pic
  | otherwise = Gloss.Blank <> pic

renderDebugTimer :: GameState -> Gloss.Picture
renderDebugTimer MkGameState{elapsedTime = time} = 
  (Gloss.color Gloss.red . Gloss.translate 10 (-25) 
  . Gloss.scale 0.25 0.25) $ Gloss.text (show time)

renderGameArea :: Gloss.Picture
renderGameArea = 
  Gloss.color (Gloss.makeColor 0 1 0 0.15) $ 
  Gloss.polygon [(0,0), (x, 0), (x, -y), (0, -y)]
  where
    x = fromIntegral $ fst gameArea
    y = fromIntegral $ snd gameArea 

renderMaze :: GameState -> Gloss.Picture -> Gloss.Picture
renderMaze MkGameState{maze = m, mazeShape = (mX, mY)} p = 
  Gloss.translate xOffset (-yOffset) $ Map.foldrWithKey f Gloss.Blank m <> p
    where 
      f k v acc = renderTile k v <> acc
      winXOffset = fst gameArea `div` 2
      mXOffset = (mX * round (fst tileSize)) `div` 2 
      xOffset = fromIntegral $ winXOffset - mXOffset
      winYOffset = snd gameArea `div` 2
      mYOffset = (mY * round (snd tileSize)) `div` 2 
      yOffset = fromIntegral $ winYOffset - mYOffset

renderTile :: TilePosition -> Tile -> Gloss.Picture
renderTile (x, y) (MkWall _) = 
  renderSquare 
    (fromIntegral x * fst tileSize, fromIntegral (-y) * snd tileSize) 
    (fst tileSize - 1, snd tileSize - 1) -- gives a tiny bit of padding for walls
    Gloss.white
renderTile _     (MkFloor _) = Gloss.Blank

-- (x, y) -> (width, height) -> Gloss.Picture
renderSquare :: (Float, Float) -> (Float, Float) -> Gloss.Color -> Gloss.Picture
renderSquare (x, y) (w, h) c = 
  Gloss.color c $ Gloss.polygon [(x, y), (x,y-h), (x+w, y-h), (x+w, y)]
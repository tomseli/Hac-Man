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
tileSize = (32, 32)

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


-- foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
renderMaze :: GameState -> Gloss.Picture -> Gloss.Picture
renderMaze MkGameState{maze=m} p = Map.foldrWithKey f Gloss.Blank m <> p
  where f k v acc = renderTile k v <> acc

renderTile :: TilePosition -> Tile -> Gloss.Picture
renderTile (x, y) (MkWall _) = 
  renderSquare (fromIntegral x * fst tileSize, fromIntegral (-y) * snd tileSize) 
  tileSize Gloss.white
renderTile _     (MkFloor _) = Gloss.Blank

-- (x, y) -> (width, height) -> Gloss.Picture
renderSquare :: (Float, Float) -> (Float, Float) -> Gloss.Color -> Gloss.Picture
renderSquare (x, y) (w, h) c = 
  Gloss.color c $ Gloss.polygon [(x, y), (x,y-h), (x+w, y-h), (x+w, y)]
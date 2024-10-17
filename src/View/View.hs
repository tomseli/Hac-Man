{-# HLINT ignore "Use bimap" #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module View.View where


import Prelude hiding (Right, Left, Up, Down)
import qualified Data.Map as Map
import qualified Graphics.Gloss.Data.Color as Gloss
import qualified Graphics.Gloss.Data.Picture as Gloss

import Controller.EntityController
import Model.Entities
    ( Direction(Down, Up, Left, Right),
      Entity(MkEntity, movement),
      EntityPosition,
      Movement(position, direction),
      Player(MkPlayer, entity) )

import Model.Maze (Tile (..), TilePosition)
import Model.Model (
  GameState (
    MkGameState,
    elapsedTime,
    enableDebug,
    maze,
    mazeShape,
    player,
    windowInfo
  ),
 )
import View.Transform (gameArea, transformPicture)

tileSize :: (Float, Float)
tileSize = (16, 16)

-- picture pipeline, add functions with signature func:: Picture -> Picture
render :: GameState -> IO Gloss.Picture
render state@MkGameState{windowInfo = wInfo, player = wPlayer} = do
  return $
    transformPicture wInfo $
      ( renderDebugInfo state
          . renderLogo
          . renderMaze state
          . renderPlayer wPlayer
      )
        Gloss.Blank

-- Add a bitmap (Add, when making the renderEntity)
renderPlayer :: Player -> Gloss.Picture -> Gloss.Picture
renderPlayer MkPlayer{entity} = renderEntity entity (Gloss.color Gloss.yellow (Gloss.ThickCircle 0 16))

renderEntity :: Entity -> Gloss.Picture  -> Gloss.Picture -> Gloss.Picture
renderEntity MkEntity{movement} bmap pic = Gloss.translate
    ((x1 * fst tileSize) + (fst tileSize /2))
    ((y1 * snd tileSize) - (snd tileSize /2))
     bmap <> pic
 where
  dir          = direction movement 
  (x1, y1)     = case dir of
                  Right -> (x, fromIntegral @Int (round y))
                  Left  -> (x, fromIntegral @Int (round y))
                  Up    -> (fromIntegral @Int (round x), y)
                  Down  -> (fromIntegral @Int (round x), y)
                  _ -> (x, y) 

  (x, y)       = position movement

renderNextPos ::  Entity -> Gloss.Picture
renderNextPos ent =
  Gloss.translate x' y'
  $ Gloss.color Gloss.blue (Gloss.ThickCircle 0 15)
  where  (x, y)     = getNextPos ent
         (x', y')   = ((x * fst tileSize) +(fst tileSize /2) ,(y * snd tileSize) - (snd tileSize /2))

renderLogo :: Gloss.Picture -> Gloss.Picture
renderLogo pic =
  (Gloss.color Gloss.white . Gloss.translate 150 (-125))
    (Gloss.color Gloss.yellow (Gloss.text "PACMAN"))
    <> pic

renderDebugInfo :: GameState -> Gloss.Picture -> Gloss.Picture
renderDebugInfo state@MkGameState{enableDebug = debug} pic
  | debug     = pic <> renderNextPos ((entity.player) state) <> renderGameArea <> 
                renderDebugTimer state 
  | otherwise = Gloss.Blank <> pic

renderDebugTimer :: GameState -> Gloss.Picture
renderDebugTimer MkGameState{elapsedTime = time} =
  ( Gloss.color Gloss.red
      . Gloss.translate 10 (-25)
      . Gloss.scale 0.25 0.25
  )
    $ Gloss.text (show time)

renderGameArea :: Gloss.Picture
renderGameArea =
  Gloss.color (Gloss.makeColor 0 1 0 0.15) $
    Gloss.polygon [(0, 0), (x, 0), (x, -y), (0, -y)]
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
renderTile _ (MkFloor _) = Gloss.Blank

-- (x, y) -> (width, height) -> Gloss.Picture
renderSquare :: (Float, Float) -> (Float, Float) -> Gloss.Color -> Gloss.Picture
renderSquare (x, y) (w, h) c =
  Gloss.color c $ Gloss.polygon [(x, y), (x, y - h), (x + w, y - h), (x + w, y)]
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

import Model.Maze (Maze, Tile (..), TilePosition)
import Model.Model (
  GameState (
    MkGameState,
    elapsedTime,
    enableDebug,
    maze,
    player,
    windowInfo
  ),
 )
import View.Transform (gameArea, tileSize, transformPicture, transformToMaze)

-- picture pipeline, add functions with signature func:: Picture -> Picture
-- pic <> should always be the left most part of these functions
render :: GameState -> IO Gloss.Picture
render state@MkGameState{windowInfo = wInfo, player = wPlayer} = do
  return $
    transformPicture wInfo $
      ( renderDebugInfo state
          . renderLogo
          . renderMaze state
          . renderPlayer wPlayer
          . renderNextPos (entity wPlayer)
      )
          Gloss.Blank

-- Add a bitmap (Add, when making the renderEntity)
-- note this is eta reduced to hell and back
renderPlayer :: Player -> Maze -> Gloss.Picture -> Gloss.Picture
renderPlayer MkPlayer{entity} = renderEntity entity

renderEntity :: Entity -> Maze -> Gloss.Picture -> Gloss.Picture -> Gloss.Picture
renderEntity MkEntity{movement} m pic bmap =
  pic <> transformToMaze m
      ( Gloss.translate
          ((x1 * fst tileSize) + (fst tileSize /2))
          ((y1 * snd tileSize) - (snd tileSize /2))
          bmap
      )
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
  pic
    <> (Gloss.color Gloss.white . Gloss.translate 150 (-125))
      (Gloss.color Gloss.yellow (Gloss.text "PACMAN"))

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
renderMaze MkGameState{maze = m} p =
  p <> transformToMaze m (Map.foldrWithKey f Gloss.Blank m)
 where
  f k v acc = renderTile k v <> acc

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
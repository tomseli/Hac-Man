{-# HLINT ignore "Use bimap" #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module View.View where

import qualified Data.Map as Map
import qualified Graphics.Gloss.Data.Color as Gloss
import qualified Graphics.Gloss.Data.Picture as Gloss
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
          . renderNextPos (entity wPlayer)
      )
        Gloss.Blank

-- Add a bitmap (Add, when making the renderEntity)
renderPlayer :: Player -> Gloss.Picture -> Gloss.Picture
renderPlayer MkPlayer{entity} = renderEntity entity

renderEntity :: Entity -> Gloss.Picture -> Gloss.Picture
renderEntity MkEntity{movement} pic =
  Gloss.translate
    ((a * fst tileSize) + (fst tileSize /2))
    ((b * snd tileSize) - (snd tileSize /2))
    $ Gloss.color Gloss.red (Gloss.ThickCircle 0 15) <> pic
 where
  (x, y)       = position movement
  (newX, newY) =  (x, y)
  (a, b)       = snapToGrid(newX, newY)

interpolateRender :: Float -> Float -> Float
interpolateRender x1 x2 =   0.5 * (x1 - x2)

snapToGrid :: EntityPosition -> EntityPosition
snapToGrid (x, y) = (fromIntegral @Int (round x), fromIntegral @Int (round y))

renderNextPos ::  Entity -> Gloss.Picture -> Gloss.Picture
renderNextPos ent pic =
  Gloss.translate x' y'
  $ Gloss.color Gloss.blue (Gloss.ThickCircle 0 15) <> pic
  where  (x', y')     = getNextPosRenderer ent

getNextPosRenderer ::  Entity -> EntityPosition
getNextPosRenderer ent = ((x-x' )* fst tileSize  , (y-y') * snd tileSize  )
                        where
                          dir        = (direction.movement) ent
                          (x , y)    = snapToGrid ((position .movement) ent)
                          (x', y')   = nPos dir
                          nPos Model.Entities.Up    = (x  ,y-1)
                          nPos Model.Entities.Left  = (x+1,  y)
                          nPos Model.Entities.Right = (x-1,  y)
                          nPos Model.Entities.Down  = (x  ,y+1)
                          nPos _                    = (x  , y)

renderLogo :: Gloss.Picture -> Gloss.Picture
renderLogo pic =
  (Gloss.color Gloss.white . Gloss.translate 150 (-125))
    (Gloss.color Gloss.yellow (Gloss.text "PACMAN"))
    <> pic

renderDebugInfo :: GameState -> Gloss.Picture -> Gloss.Picture
renderDebugInfo state@MkGameState{enableDebug = debug} pic
  | debug = renderGameArea <> renderDebugTimer state <> pic
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
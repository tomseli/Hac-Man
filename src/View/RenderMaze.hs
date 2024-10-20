module View.RenderMaze where

import qualified Data.Map as Map
import qualified Graphics.Gloss as Gloss
import Model.Maze
import View.Transform (tileSize, transformToMaze)

-- import View.Transform

straightWall :: IO Gloss.Picture
straightWall = Gloss.loadBMP "assets\\wall_straight.bmp"

cornerWall :: IO Gloss.Picture
cornerWall = Gloss.loadBMP "assets\\wall_rounded_corner.bmp"

renderMaze :: Maze -> IO Gloss.Picture -> IO Gloss.Picture
renderMaze maze pic = pic <> renderMaze' maze

renderMaze' :: Maze -> IO Gloss.Picture
renderMaze' m = do
  maze <- Map.foldrWithKey f (return Gloss.Blank) m
  return $ transformToMaze m maze
 where
  f k v acc = renderTile k v <> acc

renderTile :: TilePosition -> Tile -> IO Gloss.Picture
renderTile pos (MkWall t) = translateTile pos (renderWall t)
renderTile pos (MkFloor t) = translateTile pos (renderFloor t)

renderWall :: WallShape -> IO Gloss.Picture
renderWall (MkWallShape orient) = renderStraightWall orient
renderWall (MkCorner orient) = renderCornerWall orient

renderStraightWall :: WallOrientation -> IO Gloss.Picture
renderStraightWall Horizontal = straightWall -- default
renderStraightWall Vertical = rotateTile 90 straightWall

renderCornerWall :: CornerOrientation -> IO Gloss.Picture
renderCornerWall NE = rotateTile 180 cornerWall
renderCornerWall SE = rotateTile 270 cornerWall
renderCornerWall SW = rotateTile 0 cornerWall -- default
renderCornerWall NW = rotateTile 90 cornerWall

renderFloor :: FloorType -> IO Gloss.Picture
renderFloor EmptyTile = return Gloss.Blank
renderFloor (MkConsumable _) = return Gloss.Blank

-- A few helper functions for cleaner syntax in the main functions
translateTile :: TilePosition -> IO Gloss.Picture -> IO Gloss.Picture
translateTile (x, y) p = do
  Gloss.translate
      (fromIntegral x * fst tileSize + fst tileSize / 2)
      (fromIntegral (-y) * snd tileSize - snd tileSize / 2) <$> p

rotateTile :: Float -> IO Gloss.Picture -> IO Gloss.Picture
rotateTile deg p = do
  Gloss.rotate deg <$> p
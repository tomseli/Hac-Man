module View.RenderMaze where

import qualified Data.Map as Map
import qualified Graphics.Gloss as Gloss
import Model.Maze
import View.Transform

type Sprite = Gloss.Picture

type Sprites = (Gloss.Picture, Gloss.Picture)

-- import View.Transform

straightWall :: IO Gloss.Picture
straightWall = Gloss.loadBMP "assets\\wall_straight.bmp"

cornerWall :: IO Gloss.Picture
cornerWall = Gloss.loadBMP "assets\\wall_rounded_corner.bmp"

renderMaze :: Maze -> IO Gloss.Picture -> IO Gloss.Picture
renderMaze maze pic = pic <> renderMaze' maze

renderMaze' :: Maze -> IO Gloss.Picture
renderMaze' m = do
  putStrLn "AHHHHHHHHH"
  -- some fancy ass syntax to make a tuple as one liner
  sprites <- (,) <$> straightWall <*> cornerWall
  let maze = Map.foldrWithKey (\k v acc -> renderTile sprites k v <> acc) Gloss.Blank m
  return $ transformToMaze m maze

renderTile :: Sprites -> TilePosition -> Tile -> Gloss.Picture
renderTile sprites pos (MkWall t) = translateTile pos (renderWall t sprites)
renderTile _ pos (MkFloor t) = translateTile pos (renderFloor t Gloss.Blank)

renderWall :: WallShape -> Sprites -> Gloss.Picture
renderWall (MkWallShape orient) sprites = renderStraightWall orient (fst sprites)
renderWall (MkCorner orient) sprites = renderCornerWall orient (snd sprites)

renderStraightWall :: WallOrientation -> Gloss.Picture -> Gloss.Picture
renderStraightWall Horizontal sprite = sprite -- default
renderStraightWall Vertical sprite = Gloss.rotate 90 sprite

renderCornerWall :: CornerOrientation -> Gloss.Picture -> Gloss.Picture
renderCornerWall NE = Gloss.rotate 180
renderCornerWall SE = Gloss.rotate 270
renderCornerWall SW = Gloss.rotate 0 -- default
renderCornerWall NW = Gloss.rotate 90

renderFloor :: FloorType -> Gloss.Picture -> Gloss.Picture
renderFloor EmptyTile _ = Gloss.Blank
renderFloor (MkConsumable _) _ = Gloss.Blank

translateTile :: TilePosition -> Gloss.Picture -> Gloss.Picture
translateTile (x, y) =
  Gloss.translate
    (fromIntegral x * fst tileSize + fst tileSize / 2)
    (fromIntegral (-y) * fst tileSize - fst tileSize / 2)

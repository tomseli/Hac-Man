module View.RenderMaze where

import qualified Data.Map as Map
import qualified Graphics.Gloss as Gloss
import Model.Maze
import View.Transform

type Name = String

type Sprite = IO Gloss.Picture

type Sprites = Map.Map Name Sprite

activeSpritesPath :: [(Name, String)]
activeSpritesPath =
  [ ("straigtWall", "assets\\wall_straight.bmp")
  , ("cornerWall", "assets\\wall_rounded_corner.bmp")
  ]

loadActiveSprites :: [(Name, String)] -> Sprites
loadActiveSprites = foldr f Map.empty
 where
  f (k, v) = Map.insert k (Gloss.loadBMP v)

activeSprites :: Sprites
activeSprites = loadActiveSprites activeSpritesPath

renderMaze :: Maze -> IO Gloss.Picture -> IO Gloss.Picture
renderMaze maze pic = pic <> renderMaze' maze

renderMaze' :: Maze -> IO Gloss.Picture
renderMaze' m = do
  let mazeSprite = Map.foldrWithKey (\k v acc -> renderTile k v <> acc) (return Gloss.Blank) m
  do
    transformToMaze m <$> mazeSprite

renderTile :: TilePosition -> Tile -> Sprite
renderTile pos (MkWall t) = translateSprite pos (renderWall t)
renderTile pos (MkFloor t) = translateSprite pos (renderFloor t)

renderWall :: WallShape -> Sprite
renderWall (MkWallShape orient) = renderWallStraight orient
renderWall (MkCorner orient) = renderWallCorner orient

renderWallStraight :: WallOrientation -> Sprite
renderWallStraight Horizontal = activeSprites Map.! "straigtWall" -- default
renderWallStraight Vertical = rotateSprite 90 $ activeSprites Map.! "straigtWall"

renderWallCorner :: CornerOrientation -> Sprite
renderWallCorner NE = rotateSprite 180 $ activeSprites Map.! "cornerWall"
renderWallCorner SE = rotateSprite 270 $ activeSprites Map.! "cornerWall"
renderWallCorner SW = rotateSprite 0 $ activeSprites Map.! "cornerWall" -- default
renderWallCorner NW = rotateSprite 90 $ activeSprites Map.! "cornerWall"

renderFloor :: FloorType -> Sprite
renderFloor EmptyTile = return Gloss.Blank
renderFloor (MkConsumable _) = return $ Gloss.color Gloss.orange (Gloss.ThickCircle 0 8)

translateSprite :: TilePosition -> Sprite -> Sprite
translateSprite (x, y) s = do
  Gloss.translate
    (fromIntegral x * fst tileSize + fst tileSize / 2)
    (fromIntegral (-y) * fst tileSize - fst tileSize / 2)
    <$> s

rotateSprite :: Float -> Sprite -> Sprite
rotateSprite deg sprite = do
  Gloss.rotate deg <$> sprite

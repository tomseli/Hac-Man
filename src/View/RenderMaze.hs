module View.RenderMaze where

import qualified Data.Map as Map
import qualified Graphics.Gloss as Gloss
import Model.Maze
import Model.Model
import View.Transform

----------------------------------------------------------------------------------
-- Impure
-- Used in main to load the sprites
loadActiveSprites :: [(Name, String)] -> IO [(Name, Sprite)]
loadActiveSprites ls = do
  sprts <- loadActiveSprites' ls
  let names = map fst ls
  return (zip names sprts)

loadActiveSprites' :: [(Name, String)] -> IO [Sprite]
loadActiveSprites' = mapM f
 where
  f (_, path) = Gloss.loadBMP path

-- Pure
activeSpritesPaths :: [(Name, String)]
activeSpritesPaths =
  [ ("straigtWall", "assets\\wall_straight.bmp")
  , ("cornerWall", "assets\\wall_rounded_corner.bmp")
  -- , ("pellet", "assets\\pellet.bmp")
  -- , ("superPellet", "assets\\super_pellet.bmp")
  ]

storeActiveSprites :: [(Name, Sprite)] -> Map.Map Name Sprite
storeActiveSprites = foldr f Map.empty
 where
  f (name, sprite) = Map.insert name sprite

----------------------------------------------------------------------------------

renderMaze :: Maze -> Sprites -> Gloss.Picture -> Gloss.Picture
renderMaze m mSprites pic = pic <> renderMaze' m mSprites

renderMaze' :: Maze -> Sprites -> Gloss.Picture
renderMaze' m mSprites =
  transformToMaze
    m
    (Map.foldrWithKey (\k v acc -> renderTile k v mSprites <> acc) Gloss.Blank m)

renderTile :: TilePosition -> Tile -> Sprites -> Sprite
renderTile pos (MkWall t) mSprites = translateSprite pos (renderWall t mSprites)
renderTile pos (MkFloor t) mSprites = translateSprite pos (renderFloor t mSprites)

renderWall :: WallShape -> Sprites -> Sprite
renderWall (MkWallShape orient) mSprites = renderWallStraight orient mSprites
renderWall (MkCorner orient) mSprites = renderWallCorner orient mSprites

renderWallStraight :: WallOrientation -> Sprites -> Sprite
renderWallStraight Horizontal mSprites = mSprites Map.! "straigtWall" -- default
renderWallStraight Vertical mSprites = Gloss.rotate 90 $ mSprites Map.! "straigtWall"

renderWallCorner :: CornerOrientation -> Sprites -> Sprite
renderWallCorner NE mSprites = Gloss.rotate 180 $ mSprites Map.! "cornerWall"
renderWallCorner SE mSprites = Gloss.rotate 270 $ mSprites Map.! "cornerWall"
renderWallCorner SW mSprites = Gloss.rotate 0 $ mSprites Map.! "cornerWall" -- default
renderWallCorner NW mSprites = Gloss.rotate 90 $ mSprites Map.! "cornerWall"

renderFloor :: FloorType -> Sprites -> Sprite
renderFloor EmptyTile _ = Gloss.Blank
renderFloor (MkConsumable _) _ = Gloss.Blank

translateSprite :: TilePosition -> Sprite -> Sprite
translateSprite (x, y) =
  Gloss.translate
    (fromIntegral x * fst tileSize + fst tileSize / 2)
    (fromIntegral (-y) * fst tileSize - fst tileSize / 2)

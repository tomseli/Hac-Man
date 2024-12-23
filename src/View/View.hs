{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}

module View.View where

import           Controller.EntityController

import           Data.Foldable

import qualified Graphics.Gloss.Data.Color   as Gloss
import qualified Graphics.Gloss.Data.Picture as Gloss

import           Model.Entities
import           Model.Maze
import           Model.Model

import           Prelude                     hiding (Left, Right)

import           View.EntityAnimation
import           View.RenderMaze
import           View.SaveHighScore
import           View.Transform
-- picture pipeline, add functions with signature func:: Picture -> Picture
-- pic <> should always be the left most part of these functions
render :: GameState -> IO Gloss.Picture
render
  state@MkGameState
    { windowInfo = wInfo
    , player = player
    , maze = maze
    , ghosts = ghosts
    } =
    return $
      transformPicture wInfo $
        ( renderDebugInfo state
        . renderStatus state
        . renderLogo
        . renderPlayerAnimation player maze
        . renderGhosts ghosts maze
        . renderLevel state
        . renderPlayerScore player
        . renderHighScore state
        . renderPlayerHealth player
        . renderMaze state
        )
          Gloss.Blank

renderPlayerHealth :: Player -> Gloss.Picture -> Gloss.Picture
renderPlayerHealth MkPlayer{lives} pic =
  pic
    <> ( Gloss.color     Gloss.white
       . Gloss.translate 600 (-100)
       . Gloss.scale     0.25 0.25
       )
       (Gloss.color Gloss.white $ Gloss.text $ show lives)

renderBlinky :: Ghost -> Maze -> Gloss.Picture -> Gloss.Picture
renderBlinky MkGhost{entityG} = renderEntity entityG circle
 where
  circle = Gloss.color Gloss.red (Gloss.ThickCircle 0 24)


renderGhosts :: [Ghost] -> Maze -> Gloss.Picture -> Gloss.Picture
renderGhosts xs m pic = foldr' f pic xs
  where f x = renderGhostAnimation x m

--renders the targetTile of the ghost
renderDebugGhost :: Ghost -> Gloss.Color -> Maze -> Gloss.Picture
renderDebugGhost ghost color = renderTargetTile (targetTile ghost) circle
  where
    circle = Gloss.color color (Gloss.ThickCircle 0 16)

renderTargetTile ::
  EntityPosition -> Gloss.Picture -> Maze -> Gloss.Picture
renderTargetTile (x, y) bmap m  = transformToMaze
      m
      (Gloss.translate
      ((x * fst tileSize) + (fst tileSize / 2))
      ((y * snd tileSize) - (snd tileSize / 2))
      bmap
      )

renderPlayerScore :: Player -> Gloss.Picture -> Gloss.Picture
renderPlayerScore MkPlayer{score} pic =
  pic
    <> ( Gloss.color     Gloss.white
       . Gloss.translate 400 (-100)
       . Gloss.scale     0.25 0.25
       )
      (Gloss.color Gloss.white (Gloss.text $ show score))

renderHighScore :: GameState -> Gloss.Picture -> Gloss.Picture
renderHighScore state pic =
  pic
    <> ( Gloss.color        Gloss.white
       . Gloss.translate   50 (-100)
       . Gloss.scale       0.25 0.25
       )
      (Gloss.color Gloss.white (Gloss.text $ "Highscore: \n" ++ show (retrieveHighScore $ highscores state)))

renderLevel :: GameState -> Gloss.Picture -> Gloss.Picture
renderLevel state pic =
  pic
    <> ( Gloss.color     Gloss.white
       . Gloss.translate 50 (-950)
       . Gloss.scale     0.25 0.25
       )
      (Gloss.color Gloss.white $ Gloss.text $ "Level: \n" ++ show (level state))


renderStatus :: GameState -> Gloss.Picture -> Gloss.Picture
renderStatus MkGameState{status = GameOver} pic = renderGameOver pic
renderStatus MkGameState{status = Paused}   pic = renderPaused pic
renderStatus MkGameState{status = _}        pic = pic

renderEntity ::
  Entity -> Gloss.Picture -> Maze -> Gloss.Picture -> Gloss.Picture
renderEntity MkEntity{movement} bmap m pic =
  pic
    <> transformToMaze
      m
      ( Gloss.translate
        ((x1 * fst tileSize) + (fst tileSize / 2))
        ((y1 * snd tileSize) - (snd tileSize / 2))
        bmap
      )
 where
  dir = direction movement
  (x1, y1) = case dir of
    Right -> (x, fromIntegral @Int (round y))
    Left  -> (x, fromIntegral @Int (round y))
    Up    -> (fromIntegral @Int (round x), y)
    Down  -> (fromIntegral @Int (round x), y)
    _     -> (x, y)
  (x, y) = position movement

renderNextPos :: Entity -> Maze -> Gloss.Picture
renderNextPos ent maze =
  transformToMaze maze $
    Gloss.translate x' y' $
      Gloss.color Gloss.blue (Gloss.ThickCircle 0 15)
 where
  (x, y) = getNextPos ((position . movement) ent) ((direction . movement) ent) 0.1
  (x', y') =
    ( (x * fst tileSize) + (fst tileSize / 2)
    , (y * snd tileSize) - (snd tileSize / 2)
    )

renderPaused :: Gloss.Picture -> Gloss.Picture
renderPaused pic =
  pic
    <> renderGameArea (Gloss.makeColor 1 1 1 0.90)
    <> (Gloss.color Gloss.white . Gloss.translate 150 (-250))
        (Gloss.color Gloss.black (Gloss.text "PAUSED"))


renderGameOver :: Gloss.Picture -> Gloss.Picture
renderGameOver pic =
  pic
    <> renderGameArea (Gloss.makeColor 1 1 1 0.90)
    <> (Gloss.color Gloss.white
       . Gloss.translate 150 (-250)
       . Gloss.scale 0.50 0.50
       )
       (Gloss.color Gloss.black (Gloss.text "Game over"))

renderLogo :: Gloss.Picture -> Gloss.Picture
renderLogo pic =
  pic
    <> ( Gloss.color Gloss.white
       . Gloss.translate 300 (-62)
       . Gloss.scale 0.50 0.50
       )
       (Gloss.color Gloss.yellow (Gloss.text "PACMAN"))


renderDebugInfo :: GameState -> Gloss.Picture -> Gloss.Picture
renderDebugInfo state@MkGameState{enableDebug = debug, maze = maze, ghosts = [blinky, pinky, clyde, inky]} pic
  | debug = pic 
    <> renderNextPos ((entity . player) state) maze
    <> renderGameArea (Gloss.makeColor 0 1 0 0.15)
    <> renderDebugTimer state
    <> renderFPSCounter state
    <> renderDebugGhost blinky Gloss.red maze
    <> renderDebugGhost pinky Gloss.rose maze
    <> renderDebugGhost clyde Gloss.orange maze
    <> renderDebugGhost inky Gloss.blue maze
    <> renderMazeRenderState (isNewMaze state)
  | otherwise = Gloss.Blank <> pic
renderDebugInfo _ _ =  error "renderDebugInfo: Failed to pattern match (Missing initiated ghost?)"


renderDebugTimer :: GameState -> Gloss.Picture
renderDebugTimer MkGameState{elapsedTime = time} =
  ( Gloss.color Gloss.red
  . Gloss.translate 10 (-25)
  . Gloss.scale 0.25 0.25
  )
  $ Gloss.text $ show time

renderFPSCounter :: GameState -> Gloss.Picture
renderFPSCounter MkGameState{deltaTime = dt } =
  ( Gloss.color Gloss.red
  . Gloss.translate 10 (-60)
  . Gloss.scale 0.25 0.25
  )
  $ Gloss.text $ show $ 1/dt

-- renders a "bounding box" around what we think should be used as the game area
renderGameArea :: Gloss.Color -> Gloss.Picture
renderGameArea color =
  Gloss.color color $ 
    Gloss.polygon [(0, 0), (x, 0), (x, -y), (0, -y)]
 where
  x = fromIntegral $ fst gameArea
  y = fromIntegral $ snd gameArea

-- renders some debug info if the maze got re-rendered
renderMazeRenderState :: Bool -> Gloss.Picture
renderMazeRenderState b = Gloss.translate 575 (-25) $
                          Gloss.scale 0.25 0.25 $
                          Gloss.color Gloss.red $
                          Gloss.text ("Rendered: " ++ show b)

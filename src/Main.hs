module Main where

import Controller.Controller
import Controller.EntityController
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.IO.Game as GlossIO
import Model.CustomMaze
import Model.Model
import System.IO.Unsafe
import View.RenderMaze
import View.View

-- needed when opening in windowed mode
-- import View.Transform

window :: Gloss.Display
-- window = InWindow "Hac-Man" gameArea (0, 0)
window = Gloss.FullScreen

initialState :: GameState
initialState =
  MkGameState
    { status = Running
    , maze = customMaze
    , -- this is very stupid. But until I find an alternative, this will do
      mazePicture = unsafePerformIO $ renderMaze customMaze (return Gloss.Blank)
    , elapsedTime = 0
    , enableDebug = True
    , windowInfo = MkWindowInfo (0, 0)
    , player = testPlayer
    }

main :: IO ()
main = do
  GlossIO.playIO
    window
    Gloss.black
    60
    initialState
    render
    eventHandler
    step

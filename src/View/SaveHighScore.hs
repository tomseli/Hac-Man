{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}


module View.SaveHighScore where


import           Data.Maybe

import           Model.Model

import           Text.Read   (readMaybe)

--converts highscores into lines
loadHighScores :: String -> GameState -> GameState
loadHighScores str state = state{highscores = hscrs}
                           where hscrs = lines str

--print the highscores
debugPrinthighScores :: HighScores -> IO ()
debugPrinthighScores xs =  do mapM_ putStrLn xs

--prints highscores to file returning state
saveHighscore :: HighScores -> GameState -> IO GameState
saveHighscore hscrs state = do
                              --Stops the file from being read when we want to write (Used length to evaluate the entire list, since length tranverses the entire list)
                              length hscrs `seq` writeFile "src/highscores.txt" (unlines hscrs)
                              return state

--remembers the top 10 scores ofcourse this is useless in the current setup
--(the game doesnt render all the scores) but call it future proving
updateHighscore :: HighScores -> Int -> HighScores
updateHighscore [] nHscr            = "HIGHSCORES:" : [show nHscr]  -- Handle the case where the list is empty
updateHighscore (header:rest) nHscr =
     header : take 10 (convertToListInt (quicksort $ convertToIntList $ rest ++ [show nHscr]))

--From the book: "Programming in Haskell" by Graham Hutton, Chapter Introduction
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted  = quicksort [a | a <- xs, a > x]
    in  biggerSorted ++ [x] ++ smallerSorted


--save conversion from string to Int since read is undefined with an empty string
convertToIntList :: [String] -> [Int]
convertToIntList = map (fromMaybe 0 . readMaybe)

convertToListInt :: [Int] -> [String]
convertToListInt = map show

--save function to transform highscores into a single Integer (no. 1 highscore)
retrieveHighScore :: HighScores -> Int
retrieveHighScore []        = 0
retrieveHighScore (_:score) = (fromMaybe 0 . readMaybe) $ fromMaybe [] (listToMaybe score)


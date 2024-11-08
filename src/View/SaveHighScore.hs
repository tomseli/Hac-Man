{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}


module View.SaveHighScore where


import           Data.Maybe

import           Model.Model

import           Text.Read   (readMaybe)

loadHighScores :: String -> GameState -> GameState
loadHighScores str state = state{highscores = hscrs}
                           where hscrs = lines str

--print the highscores
debugPrinthighScores :: HighScores -> IO ()
debugPrinthighScores xs =  do mapM_ putStrLn xs

saveHighscore :: HighScores -> GameState -> IO GameState
saveHighscore hscrs state = do
                              --closes file and write to it
                              length hscrs `seq` writeFile "src/highscores.txt" (unlines hscrs)
                              return state

--remembers the top 10 scores ofcourse this is useless in the current setup
--(the game doesnt render all the scores) but call it future proving
updateHighscore :: HighScores -> Int -> HighScores
updateHighscore [] nHscr            = "HIGHSCORES:" : [show nHscr]  -- Handle the case where the list is empty
updateHighscore (header:rest) nHscr =
     header : take 10 (convertToListInt (quicksort $ convertToIntList $ rest ++ [show nHscr]))

--uit t boek
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

retrieveHighScore :: HighScores -> Int
retrieveHighScore []        = 0
retrieveHighScore (_:score) = (fromMaybe 0 . readMaybe) $ fromMaybe [] (listToMaybe score)


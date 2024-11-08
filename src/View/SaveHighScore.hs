{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}


module View.SaveHighScore where


import           Model.Model


loadHighScores :: String -> GameState -> GameState
loadHighScores str state = state{highscores = hscrs}
                           where hscrs = lines str

--print the highscores
debugPrinthighScores :: HighScores -> IO ()
debugPrinthighScores xs =  do mapM_ putStrLn xs

saveHighscore :: HighScores -> GameState -> IO GameState
saveHighscore hscrs state = do
                              writeFile "src/highscores.txt" (unlines hscrs)
                              return state

--remembers the top 10 scores
updateHighscore :: [String] -> Int -> [String]
updateHighscore [] nHscr            = "HIGHSCORES:" : [show nHscr]  -- Handle the case where the list is empty
updateHighscore (header:rest) nHscr = header : take 10 (convertToListInt (quicksort $ convertToIntList $ rest ++ [show nHscr]))

--uit t boek
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted  = quicksort [a | a <- xs, a > x]
    in  biggerSorted ++ [x] ++ smallerSorted

convertToIntList :: [String] -> [Int]
convertToIntList = map read

convertToListInt :: [Int] -> [String]
convertToListInt = map show

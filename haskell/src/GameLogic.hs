module GameLogic (checkGuess) where

import Data.List (elemIndex)

-- Function to check if the word is correct
checkGuess :: String -> String -> String
checkGuess target guess 
    |length guess < length target = "guess is too short"
    | length guess > length target = "guess is too long"
    | otherwise = map checkLetter (zip target guess)
  where
    checkLetter (t, g)
      | t == g    = '@'  -- Correct letter and position
      | g `elem` target = 'O'  -- Correct letter, wrong position
      | otherwise = 'X'  -- Wrong letter
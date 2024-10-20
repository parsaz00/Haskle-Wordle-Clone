module Main where

import System.IO (hFlush, stdout)
import Data.List (elemIndex)

-- Function to check if the word is correct
checkGuess :: String -> String -> [Char]
checkGuess target guess = map checkLetter (zip target guess)
  where
    checkLetter (t, g)
      | t == g    = '@'  -- Correct letter and position
      | g `elem` target = 'O'  -- Correct letter, wrong position Reference for elem function : https://zvon.org/other/haskell/Outputprelude/elem_f.html
      | otherwise = 'X'  -- Wrong letter

-- Function to run one round of the game
runGame :: String -> IO ()
runGame targetWord = do
    putStrLn "Enter your guess: "
    guess <- getLine
    let feedback = checkGuess targetWord guess
    putStrLn feedback
    if feedback == replicate (length targetWord) '@'
      then putStrLn ("Congratulations, you got the word " ++ targetWord ++ " right!")
      else putStrLn "Try again!"

-- Function to handle multiple attempts (POC allows only one attempt)
playWordle :: IO ()
playWordle = do
    let targetWord = "haskell"  -- Hardcoded for now
    runGame targetWord
    putStrLn "Play again? (y/n)"
    response <- getLine
    if response == "y"
      then playWordle
      else putStrLn "Thanks for playing!"

main :: IO ()
main = playWordle

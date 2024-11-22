module Main where

import System.IO (hFlush, stdout)
import GameLogic (checkGuess)
import GUI (launchGUI)

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
-- Terminal based
-- main = playWordle

-- Gui Based
main = launchGUI
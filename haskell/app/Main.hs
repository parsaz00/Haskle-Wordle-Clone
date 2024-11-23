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


-- Dictionary of alphabet letters and corresponding riddles with CS terms
riddles :: [(Char, String)]
riddles = 
    [ ('a', "I am a step-by-step set of instructions to solve problems. What is my first letter?"),
      -- Algorithm
      ('b', "I am a data structure that branches like a tree. What is my first letter?"), 
      -- Binary Tree
      ('c', "I am the brain of the computer, handling all instructions. What is my first letter?"),
      -- CPU
      ('d', "I store key-value pairs for quick lookups. What is my first letter?"),
      -- Dictionary
      ('e', "I am an exceptionally frequent type of error programmers encounter. What is my first letter?"),
      -- Exception
      ('f', "I refer to a function you pass as an argument to another function. What is my first letter?"),
      -- First-Class Function
      ('g', "I am a way to combine multiple nodes and edges to model relationships. What is my first letter?"),
      -- Graph
      ('h', "I am a table that maps keys to values using a hash function. What is my first letter?"),
      -- Hash Table
      ('i', "I stand for structured storage used to fetch or update data quickly. What is my first letter?"),
      -- Index
      ('j', "I am a widely used language for web development. What is my first letter?"),
      -- JavaScript
      ('k', "I manage containerized applications. What is my first letter?"),
      -- Kubernetes
      ('l', "I am a greek letter often used in some type of calculus. What is my first letter?"),
      -- Lambda
      ('m', "I process data by feeding it through interconnected nodes. What is my first letter?"),
      -- Machine Learning Model
      ('n', "I allow devices to communicate within a given range. What is my first letter?"),
      -- Network
      ('o', "I am a technique to improve program efficiency. What is my first letter?"),
      -- Optimization
      ('p', "I am a language that powers dynamic content on the web. What is my first letter?"),
      -- Python
      ('q', "I process tasks by dividing them into smaller independent units. What is my first letter?"),
      -- Queue
      ('r', "I am a widely used technique to fetch data from databases. What is my first letter?"),
      -- Query
      ('s', "I am a way to organize and manage data using a schema. What is my first letter?"),
      -- SQL
      ('t', "I execute instructions one after the other in a program. What is my first letter?"),
      -- Thread
      ('u', "I describe the quality of user experience in software design. What is my first letter?"),
      -- Usability
      ('v', "I manage code versions and collaborative work. What is my first letter?"),
      -- Version Control
      ('w', "I help build user interfaces with reusable components. What is my first letter?"),
      -- Widget
      ('x', "I help make sure code doesn't break as new changes are introduced. What is my first letter?"),
      -- Testing
      ('y', "I am a tool for representing data graphically. What is my first letter?"),
      -- YAML
      ('z', "I compress data without losing information. What is my first letter?")
      -- Zip
    ]

-- main_v2 game loop
main_v2 :: IO ()
main_v2 = do
    let secretWord = "happy"  
    let secretLength = length secretWord
    putStrLn "Welcome to Wordle! Type 'exit' to quit."
    let gameLoop = do
            putStr ("Enter your "++ show secretLength ++"-letter guess or type 'exit' to quit: ")
            guess <- getLine
            if guess == "exit"
                then putStrLn "Thanks for playing! Goodbye."
                else if length guess /= secretLength
                    then do
                        putStrLn ("Your guess must be exactly "++ show secretLength ++" letters long.")
                        gameLoop
                    else do
                        let feedback = giveFeedback secretWord guess
                        putStrLn ("Feedback: " ++ feedback ++ "\n" ++ "\n" ++
                                 "Detailed Feedback below." ++ "\n" ++ 
                                 "You have: " ++ show (charCount '@' feedback) ++ " correctly guessed characters."++ "\n" ++ 
                                 "@ in the Feedback string denotes a character correctly guessed (and at the right place)." ++ "\n" ++
                                 "0 in the Feedback string denotes the position of a character in the guess string that is in a different position in the wordle word." ++
                                 "\n" ++ "X in the Feedback string denotes an incorrect character guess")
                        if feedback == replicate (length secretWord) '@'
                            then putStrLn "Congratulations! You've guessed the word of the day!" >> putStrLn "Bye."
                            else do
                                let firstIncorrect = getFirstIncorrectLetter secretWord guess
                                case firstIncorrect of
                                    Just letter -> putStrLn ("\n" ++ "Hint: " ++ getRiddle letter)
                                    Nothing -> return ()
                                gameLoop
    gameLoop

-- Helper to count number of ocurrences of chosenCharacter in a string
charCount :: Char -> String -> Int
charCount chosenCharacter s = length (filter (== chosenCharacter) s)

-- Helper to get riddle for a given letter
getRiddle :: Char -> String
getRiddle letter = 
    case lookup letter riddles of
        Just riddle -> riddle
        Nothing     -> "No riddle available for this letter."

-- Helper to give feedback on the guess
giveFeedback :: String -> String -> String
giveFeedback secret guess = 
    let feedback = [if guess !! i == secret !! i then '@' else if guess !! i `elem` secret then '0' else 'X' | i <- [0..length guess - 1]]
    in feedback

-- Helper to find the first incorrect letter for a hint
getFirstIncorrectLetter :: String -> String -> Maybe Char
getFirstIncorrectLetter secret guess = 
    let zippedPairs = zip secret guess
        incorrectLetters = [s | (s, g) <- zippedPairs, s /= g]
    in if null incorrectLetters then Nothing else Just (head incorrectLetters)
    

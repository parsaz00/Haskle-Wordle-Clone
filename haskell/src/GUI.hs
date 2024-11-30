module GUI where

import Graphics.UI.Gtk
import GameLogic (checkGuess)
import Control.Monad (zipWithM_, when)
import Data.IORef
import System.Random (randomRIO)

-- Example function to generate a random word
generateNewTargetWord :: FilePath -> IO (Maybe (String, String))
generateNewTargetWord filePath = do
  fileContent <- readFile filePath
  let wordList = map parseLine (lines fileContent)
  if null wordList
    then return Nothing
    else do
      randomIndex <- randomRIO (0, length wordList - 1)
      return (Just (wordList !! randomIndex))
  where
    parseLine line = 
      let parts = words line
      in (head parts, unwords (tail parts)) -- Splits the line into word and difficulty

-- Main function to launch the GUI
launchGUI :: String -> String -> IO ()
launchGUI targetWord difficulty = do
  -- Initialize GTK
  initGUI

  -- Create the main window
  window <- windowNew
  set window [windowTitle := "Wordle Game", windowDefaultWidth := 400, windowDefaultHeight := 600]

  -- Create a vertical box layout
  vbox <- vBoxNew False 10
  containerAdd window vbox

  -- Create a grid for displaying guesses
  grid <- tableNew 6 (length targetWord) True
  boxPackStart vbox grid PackGrow 0

  -- Populate the grid with empty labels
  labels <- mapM (mapM (\_ -> labelNew (Just "_"))) (replicate 6 (replicate (length targetWord) ()))
  zipWithM_ (\row rowWidgets -> 
    zipWithM_ (\col widget -> 
      tableAttachDefaults grid widget col (col+1) row (row+1)) 
      [0..] rowWidgets) 
      [0..] labels

  -- Add a text entry for guesses
  input <- entryNew
  boxPackStart vbox input PackNatural 10

  -- Add a submit button
  submitButton <- buttonNewWithLabel "Submit"
  boxPackStart vbox submitButton PackNatural 10

  -- Add a hint button
  hintButton <- buttonNewWithLabel "Hint"
  boxPackStart vbox hintButton PackNatural 10

  -- Track remaining hints
  remainingHints <- newIORef 3

  -- Create a label to display feedback
  feedbackLabel <- labelNew (Just "Enter your guess!")
  boxPackStart vbox feedbackLabel PackNatural 10

  -- Create a label to display difficulty
  difficultyLabel <- labelNew ((Just $ "Difficulty: " ++ difficulty))
  boxPackStart vbox difficultyLabel PackNatural 10

  -- Add a legend to explain the colors
  legendBox <- hBoxNew False 10
  boxPackStart vbox legendBox PackNatural 10

  -- Add legend items
  greenLegend <- labelNew (Just "🟩 Correct letter & position")
  yellowLegend <- labelNew (Just "🟨 Correct letter, wrong position")
  grayLegend <- labelNew (Just "⬜ Incorrect letter")

  boxPackStart legendBox greenLegend PackNatural 5
  boxPackStart legendBox yellowLegend PackNatural 5
  boxPackStart legendBox grayLegend PackNatural 5

  -- Track the current row of guesses
  currentRow <- newIORef 0

  -- Add a "Retry" button (initially hidden)
  retryButton <- buttonNewWithLabel "New Game"
  boxPackStart vbox retryButton PackNatural 10
  widgetHide retryButton

  -- Handle submit button click
  on submitButton buttonActivated $ do
    guess <- entryGetText input
    row <- readIORef currentRow
    if row >= length labels
      then do
        labelSetText feedbackLabel "Game Over! You've used all your guesses."
        widgetShow retryButton
      else if length guess /= length targetWord
        then labelSetText feedbackLabel $ "Guess must be " ++ show (length targetWord) ++ " letters long!"
        else do
          -- Get feedback from the game logic
          let feedback = checkGuess targetWord guess

          -- Update the grid with feedback
          zipWithM_ (\col char -> labelSetText (labels !! row !! col) [char]) [0..length targetWord - 1] guess

          -- Show feedback using colored symbols (e.g., emojis)
          zipWithM_ (\col char -> do
            let label = labels !! row !! col
            case char of
              '@' -> labelSetText label "🟩"  -- Green square
              'O' -> labelSetText label "🟨"  -- Yellow square
              'X' -> labelSetText label "⬜"  -- Gray square
            ) [0..length targetWord - 1] feedback

          -- Update feedback label
          if feedback == replicate (length targetWord) '@'
            then do
              labelSetText feedbackLabel "Congratulations, you guessed the word!"
              widgetShow retryButton
            else do
              labelSetText feedbackLabel "Try again!"
              modifyIORef currentRow (+1)
    
  -- Handle hint button click
  on hintButton buttonActivated $ do
    hintsLeft <- readIORef remainingHints
    if hintsLeft <= 0
      then labelSetText feedbackLabel "No hints left!"
      else do
        -- Generate a random hint
        randomIndex <- randomRIO (0, length targetWord - 1)
        let hint = "Hint: The letter at position " ++ show (randomIndex + 1) ++ " is '" ++ [targetWord !! randomIndex] ++ "'." ++ "\n" ++ "Riddle: " ++ getRiddle4GUI targetWord ++ "."
        labelSetText feedbackLabel hint
        modifyIORef remainingHints (\x -> x - 1)  

-- Handle retry button click
  on retryButton buttonActivated $ do
    widgetDestroy window -- Close the current window
    result <- generateNewTargetWord "haskell/Words.txt"
    case result of
      Nothing -> putStrLn "Error: Words file could not be loaded or is empty."
      Just (newTargetWord, newDifficulty) -> launchGUI newTargetWord newDifficulty

  -- Show everything and start the GTK main loop
  widgetShowAll window
  on window objectDestroy mainQuit
  mainGUI

riddles4GUI :: [(String, String)]
riddles4GUI = 
    [ ("algorithm", "I am the foundation of any program, guiding the process step by step."),
      ("binary", "I am a fundamental data structure branching out like limbs."), 
      ("cpu", "I am the processing unit that executes instructions."),
      ("dictionary", "I am a data structure used for quick key-based lookups."),
      ("exception", "I represent an unexpected condition in a program's execution."),
      ("function", "I represent a reusable block of code in programming."),
      ("graph", "I am a structure made of nodes and edges."),
      ("hash", "I map keys to values using a specialized function."),
      ("index", "I am a structured lookup system to fetch or locate data."),
      ("java", "I am a widely-used programming language for building websites."),
      ("kernel", "I am the core of an operating system managing resources."),
      ("lambda", "I am a Greek letter often used in functional programming."),
      ("model", "I am a mathematical representation used to make predictions."),
      ("network", "I enable devices to exchange data and communicate."),
      ("optimization", "I am the act of improving performance or efficiency."),
      ("python", "I am a popular language for scripting and dynamic applications."),
      ("queue", "I am a structure for managing tasks in a specific order."),
      ("query", "I represent a method for retrieving data."),
      ("sql", "I am a standard language used to interact with databases."),
      ("thread", "I allow concurrent execution of code sequences."),
      ("usability", "I describe the quality of interaction between a user and a system."),
      ("version", "I help track changes in source code over time."),
      ("widget", "I am a reusable component in graphical user interfaces."),
      ("testing", "I ensure that new changes don't break existing functionality."),
      ("yaml", "I am a data-serialization standard often used for configuration files."),
      ("zip", "I am a method to compress files into smaller sizes.")
    ]

-- Helper to get riddle for a the wordleWordGUI
getRiddle4GUI :: String -> String
getRiddle4GUI wordleWord
    | Just riddle <- lookup wordleWord riddles4GUI = riddle
    | otherwise = "No riddle available for this word."

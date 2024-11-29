-- module GUI where

-- import Graphics.UI.Gtk
-- import GameLogic (checkGuess)
-- import Control.Monad (zipWithM_, when)
-- import Data.IORef

-- -- Main function to launch the GUI
-- launchGUI :: IO ()
-- launchGUI = do
--   -- Initialize GTK
--   initGUI

--   -- Create the main window
--   window <- windowNew
--   set window [windowTitle := "Wordle Game", windowDefaultWidth := 400, windowDefaultHeight := 600]

--   -- Create a vertical box layout
--   vbox <- vBoxNew False 10
--   containerAdd window vbox

--   -- Create a grid for displaying guesses
--   grid <- tableNew 6 5 True
--   boxPackStart vbox grid PackGrow 0

--   -- Populate the grid with empty labels
--   labels <- mapM (mapM (\_ -> labelNew (Just "_"))) (replicate 6 (replicate 5 ()))
--   zipWithM_ (\row rowWidgets -> zipWithM_ (\col widget -> tableAttachDefaults grid widget col (col+1) row (row+1)) [0..] rowWidgets) [0..] labels

--   -- Add a text entry for guesses
--   input <- entryNew
--   boxPackStart vbox input PackNatural 10

--   -- Add a submit button
--   submitButton <- buttonNewWithLabel "Submit"
--   boxPackStart vbox submitButton PackNatural 10

--   -- Create a label to display feedback
--   feedbackLabel <- labelNew (Just "Enter your guess!")
--   boxPackStart vbox feedbackLabel PackNatural 10

--   -- Track the current row of guesses
--   let targetWord = "haskell" -- Hardcoded for now
--   currentRow <- newIORef 0

--   -- Handle submit button click
--   on submitButton buttonActivated $ do
--     guess <- entryGetText input
--     row <- readIORef currentRow
--     if row >= length labels
--       then labelSetText feedbackLabel "Game Over! You've used all your guesses."
--       else if length guess /= length targetWord
--         then labelSetText feedbackLabel "Guess must be 7 letters long!"
--         else do
--           -- Get feedback from the game logic
--           let feedback = checkGuess targetWord guess

--           -- Ensure feedback and guess lengths match
--           let validGuess = take (length (head labels)) guess
--               validFeedback = take (length (head labels)) feedback

--           -- Update the grid with feedback
--           zipWithM_ (\col char -> when (col < length (head labels)) $
--             labelSetText (labels !! row !! col) [char]) [0..] validGuess

--           -- Show feedback using colored symbols (e.g., emojis)
--           zipWithM_ (\col char -> when (col < length (head labels)) $ do
--             let label = labels !! row !! col
--             case char of
--               '@' -> labelSetText label "🟩"  -- Green square
--               'O' -> labelSetText label "🟨"  -- Yellow square
--               'X' -> labelSetText label "⬜"  -- Gray square
--             ) [0..] validFeedback

--           -- Update feedback label
--           if feedback == replicate (length targetWord) '@'
--             then labelSetText feedbackLabel "Congratulations, you guessed the word!"
--             else do
--               labelSetText feedbackLabel "Try again!"
--               modifyIORef currentRow (+1)

--   -- Show everything and start the GTK main loop
--   widgetShowAll window
--   on window objectDestroy mainQuit
--   mainGUI
module GUI where

import Graphics.UI.Gtk
import GameLogic (checkGuess)
import Control.Monad (zipWithM_, when)
import Data.IORef
import System.Random (randomRIO)

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
  grid <- tableNew 6 7 True
  boxPackStart vbox grid PackGrow 0

  -- Populate the grid with empty labels
  labels <- mapM (mapM (\_ -> labelNew (Just "_"))) (replicate 6 (replicate 7 ()))
  zipWithM_ (\row rowWidgets -> zipWithM_ (\col widget -> tableAttachDefaults grid widget col (col+1) row (row+1)) [0..] rowWidgets) [0..] labels

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
  -- Load the words from the file
  currentRow <- newIORef 0

  -- Handle submit button click
  on submitButton buttonActivated $ do
    guess <- entryGetText input
    row <- readIORef currentRow
    if row >= length labels
      then labelSetText feedbackLabel "Game Over! You've used all your guesses."
      else if length guess /= length targetWord
        then labelSetText feedbackLabel $ "Guess must be " ++ show (length targetWord) ++ " letters long!"
        else do
          -- Get feedback from the game logic
          let feedback = checkGuess targetWord guess

          -- Ensure feedback and guess lengths match
          let validGuess = take (length (head labels)) guess
              validFeedback = take (length (head labels)) feedback

          -- Update the grid with feedback
          zipWithM_ (\col char -> when (col < length (head labels)) $
            labelSetText (labels !! row !! col) [char]) [0..] validGuess

          -- Show feedback using colored symbols (e.g., emojis)
          zipWithM_ (\col char -> when (col < length (head labels)) $ do
            let label = labels !! row !! col
            case char of
              '@' -> labelSetText label "🟩"  -- Green square
              'O' -> labelSetText label "🟨"  -- Yellow square
              'X' -> labelSetText label "⬜"  -- Gray square
            ) [0..] validFeedback

          -- Update feedback label
          if feedback == replicate (length targetWord) '@'
            then labelSetText feedbackLabel "Congratulations, you guessed the word!"
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
        let hint = "Hint: The letter at position " ++ show (randomIndex + 1) ++ " is '" ++ [targetWord !! randomIndex] ++ "'."
        labelSetText feedbackLabel hint
        modifyIORef remainingHints (\x -> x - 1)  

  -- Show everything and start the GTK main loop
  widgetShowAll window
  on window objectDestroy mainQuit
  mainGUI
            
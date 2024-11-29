# Haskle: Final Proposal


# Guide to MVP + Learning done

"Parsa + Daichi"

One of the main things our group had to learn to be able to build out our MVP was to learn how to create a GUI in Haskell. This was a challenging undertaking, but we had a lot of fun (and a bit of stress) creating it. As we know, Haskeell is a functional language, meaning it emphasizes immutability and pure functions, which in turn means that functions have no side effects. This obivously presents a challenge, because a GUI is the "king" of side effects. Furthermore, Haskell doesn't have a mainstream or well agreed upon native GUI framework built into its ecosystem. So we had to learn and rely on the GTK+3 toolkit to be able to build out the GUI. By using the `gtk3` library, we were able to leverage its framework for GUI creation, as it provides Haskell bindings to GTK+3, allowing us to access its widgets and event-driven programming model. 

## Code snippets and associated learning for the GUI
```
-- Main function to launch the GUI
launchGUI :: String -> String -> IO ()
launchGUI targetWord difficulty = do
  -- Initialize GTK
  initGUI

  -- Create the main window
  window <- windowNew
  set window [windowTitle := "Wordle Game", windowDefaultWidth := 400, windowDefaultHeight := 600]
```

  https://github.students.cs.ubc.ca/parsaz00/cpsc-312-project/blob/6ac6833e59d534a69001f8a0cafb7d51132e451c/haskell/src/GUI.hs#L9C1-L17C99

  Here, we start with initializng the library with initGUI. Since we are working with a GUI (which will have many side effects) we need all the the GUI actions to be side effects, which makes the use of IO monads perfect, so that we can handle all the side effects appropriately. Furthermore, the way our GUI works is almost hierarchical in terms of creating the different aspects of it. Since the GUI requires specific sequences of operations (ex: creating a window, then adding a button, then showing the window), the Monad and its natural handling of this flow of operations was perfect. Using online tools like the documentation for gtk3 and chatGPT, we were able to figure out how to initialize our GUI. Finally, set window [windowTitle := "Wordle Game", windowDefaultWidth := 400, windowDefaultHeight := 600] is used to actually set the properties of our GUI window, including the title and the dimensions. 

```
-- Create a vertical box layout
vbox <- vBoxNew False 10
containerAdd window vbox
```

```
-- Create a grid for displaying guesses
grid <- tableNew 6 7 True
boxPackStart vbox grid PackGrow 0
```

https://github.students.cs.ubc.ca/parsaz00/cpsc-312-project/blob/6ac6833e59d534a69001f8a0cafb7d51132e451c/haskell/src/GUI.hs#L19C3-L25C36

Here, we began actually populating our GUI window. vBoxNew False 10 creates a vertical box layout, which is a container that arranges child widgets in a vertical line below each other. The first argument False is so that the widgets retain their true size and are not set to be equally distributed. 10 is the spacing in pixels that we want between the child widget. We had to play around with this and get advice from ChatGPT until it looked right. 
containerAdd window vbox adds the vertical box we just made to the window 

```
grid <- tableNew 6 7 True
```

We create a grid with 6 rows and 7 columns. By using True, we indicate that the rows and columns should ALL have the same size.

```
boxPackStart vbox grid PackGrow 0
```

We add the grid to the vbox. PackGrow allows the vbox to expand the grid and fill any extra space if the window is resized . 0 means no extra padding around the grid 

Overall, by this point, we actually began to apprecaite the inherent flow of operations that the Haskell monad operates in. The way we are building the GUI up comes really naturally , because it has a logical flow to it. We are declaring a layout: Add X to the vbox, and the vbox to the window etc. 

```
-- Create a 6x7 grid of label widgets initialized with "_"
labels <- mapM (mapM (\_ -> labelNew (Just "_"))) (replicate 6 (replicate 7 ()))

-- Attach each label to its corresponding position in the grid
zipWithM_ (\row rowWidgets ->
    zipWithM_ (\col widget ->
        tableAttachDefaults grid widget col (col+1) row (row+1))
    [0..] rowWidgets)
    [0..] labels
```
https://github.students.cs.ubc.ca/parsaz00/cpsc-312-project/blob/6ac6833e59d534a69001f8a0cafb7d51132e451c/haskell/src/GUI.hs#L27C2-L33C19

```
labels <- mapM (mapM (\_ -> labelNew (Just "_"))) (replicate 6 (replicate 7 ()))
```

replicate 6 (replicate 7 ()): this creates a list of lists filled with (). This will be placeholders for the rows and columns 


(\_ -> labelNew (Just "_")): this creates a new GTK label widget, which we initialize with “_”
These are empty placeholders in the grid, where the guesses will ultimately appears

```
mapM (mapM ...):
```

The first mapM applies a monadic action to each row in the list, meaning we fill each row in the grid with (“_”)
The second map applies the same action to every placeholder in a row
zipWithM_ is like zipWith, but the monadic version of it. It performs the same pairwise combination, but allows side effects (in this case updating our gui) since it operates in a monad . We use them like a nested loop. The outer zipWithM_ iterates over the rows and then inner zipWithM_ iterates over columns within a row
```
zipWithM_ (\row rowWidgets ...) [0..] labels
```

Labels is our 6x7 grid of widgets [0..] are the indices 

With this we iterate over each row index (row) and its corresponding row of widgets (rowWidgets)
```
zipWithM_ (\col widget ...) [0..] rowWidgets
```

For each row of widgets, in the inner lop we iterate over the columns indices (col) and process each widget in that row → we attach the widget to grid at position (row, col)

```
tableAttachDefaults grid widget col (col+1) row (row+1)
```

This actually places the widget in the grid at the specific row and column. grid is the grid container. Widget is the label we want to attach. Finally, col,col+1,row,row+1 are the starting and ending column and row indices respectively. 


```
-- Add a text entry for guesses
input <- entryNew
boxPackStart vbox input PackNatural 10

-- Add a submit button
submitButton <- buttonNewWithLabel "Submit"
boxPackStart vbox submitButton PackNatural 10

-- Add a hint button
hintButton <- buttonNewWithLabel "Hint"
boxPackStart vbox hintButton PackNatural 1
```
https://github.students.cs.ubc.ca/parsaz00/cpsc-312-project/blob/6ac6833e59d534a69001f8a0cafb7d51132e451c/haskell/src/GUI.hs#L35C3-L45C46

Overall, we are adding interactive elements to the GUI here. We create a text input widget where users will be able to enter their text, and then we add it to our vertical box. We also create a button labeled "Submit" and add it to the vertical box. Finally, we create a Hint button, which we add to the vertical box. This is the button users will press if they want to get a hint. 


```
legendBox <- hBoxNew False 10
boxPackStart vbox legendBox PackNatural 10

greenLegend <- labelNew (Just "🟩 Correct letter & position")
yellowLegend <- labelNew (Just "🟨 Correct letter, wrong position")
grayLegend <- labelNew (Just "⬜ Incorrect letter")

boxPackStart legendBox greenLegend PackNatural 5
boxPackStart legendBox yellowLegend PackNatural 5
boxPackStart legendBox grayLegend PackNatural 5
```

Here, we are simply populating our GUI window with a legend that will tell users what the feedback they recieve from their guess actually means. 

```
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
            then labelSetText feedbackLabel "Congratulations, you guessed the word!"
            else do
              labelSetText feedbackLabel "Try again!"
              modifyIORef currentRow (+1)
```

This is the core part of the program's interactivity. Here, we are processing user input and then updating the GUI in response to button clicks. 

```
on submitButton buttonActivated $ do
```

Here, we learned about attaching event listeners (similar to what we had done in 210 with our GUI's). Specifically here, we are attaching an event listener to the submitButton. buttonActivated specifies that the event type is a click of the button. Finally $do starts the block of actions that should be executed when the button is clicked. 

```
guess <- entryGetText input
```
Read the text entered in the input field and store it in guess for further processing.

```
row <- readIORef currentRow
``` 
Reads the current value of the mutable currentRow (the index of the grid row being updated)

```
	if row >= length labels
 		 then labelSetText feedbackLabel "Game Over! You've used all your guesses."
 else if length guess /= length targetWord
    			then labelSetText feedbackLabel $ "Guess must be " ++ show (length targetWord) ++ " letters long!"
```

If the user has already used all the rows (i.e., used up all their guesses) then the game is over. In the second part, if the guess length is not correct, we inform the user by updating the GUI with the feedbackLabel 

```
else do
  let feedback = checkGuess targetWord guess
```

This part is us calling the checkGuess function in GameLogic, which we developed for the POC

```
zipWithM_ (\col char -> labelSetText (labels !! row !! col) [char]) [0..] guess
```

Iterate through each column index (col) and corresponding letter (char) in the guess 

labels !! row !! col : this selects the label at the current row and column 
Finally, labelSetText updates the label to display the letter from the guess 

```
zipWithM_ (\col char -> case char of
  '@' -> labelSetText (labels !! row !! col) "🟩"
  'O' -> labelSetText (labels !! row !! col) "🟨"
  'X' -> labelSetText (labels !! row !! col) "⬜"
  ) [0..] feedback
```

We iterate through the indices of feedback and update the grid according to the map with the correct color box emoji . We then call modifyIORef currentRow (+1). This updates the mutable value of currentRow by incrementing it. This ensures that the next guess will appear in the following row 


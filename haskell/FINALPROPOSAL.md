# Haskle: Final Proposal


# Guide to MVP + Learning done

"Parsa + Daichi"

One of the main things our group had to learn to be able to build out our MVP was to learn how to create a GUI in Haskell. This was a challenging undertaking, but we had a lot of fun (and a bit of stress) creating it. As we know, Haskeell is a functional language, meaning it emphasizes immutability and pure functions, which in turn means that functions have no side effects. This obivously presents a challenge, because a GUI is the "king" of side effects. Furthermore, Haskell doesn't have a mainstream or well agreed upon native GUI framework built into its ecosystem. So we had to learn and rely on the GTK+3 toolkit to be able to build out the GUI. By using the `gtk3` library, we were able to leverage its framework for GUI creation, as it provides Haskell bindings to GTK+3, allowing us to access its widgets and event-driven programming model. 

## Code snippets and associated learning for the GUI
-- Main function to launch the GUI
launchGUI :: String -> String -> IO ()
launchGUI targetWord difficulty = do
  -- Initialize GTK
  initGUI

  -- Create the main window
  window <- windowNew
  set window [windowTitle := "Wordle Game", windowDefaultWidth := 400, windowDefaultHeight := 600]

  Here, we start with initializng the library with initGUI. Since we are working with a GUI (which will have many side effects) we need all the the GUI actions to be side effects, which makes the use of IO monads perfect, so that we can handle all the side effects appropriately. Furthermore, the way our GUI works is almost hierarchical in terms of creating the different aspects of it. Since the GUI requires specific sequences of operations (ex: creating a window, then adding a button, then showing the window), the Monad and its natural handling of this flow of operations was perfect. Using online tools like the documentation for gtk3 and chatGPT, we were able to figure out how to initialize our GUI. Finally, set window [windowTitle := "Wordle Game", windowDefaultWidth := 400, windowDefaultHeight := 600] is used to actually set the properties of our GUI window, including the title and the dimensions. 

# "Haskle: Cracking Words in the Terminal - A Haskell-Powered Wordle Adventure!"
This project brings the beloved word-guessing game "Wordle" to life in Haskell, offering a fun and challenging experience right from the terminal. Players will test their deduction skills as they try to uncover a hidden word, with each guess receiving instant feedback—letters in the correct spot, misplaced letters, and letters that don’t belong. What makes _Haskle_ special is how we’ll use Haskell's functional programming strengths to elegantly handle the game’s logic and user interactions, creating an experience that’s both intuitive and rewarding for fans of the original game!

This project is in fulfillment of the [CPSC 312 2024W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website-2024W1/project.html).
# Team Members
Our team is:

+ Daichi Furukawa (student # 51399111)
+ Yuvraj Upadhyay (student # 47253539): theonewhocanread
+ Amar Gill (student # 51913747)
+ Parsa Seyed Zehtab # 84226935
+ If you have a fifth member: Team Member Name 5 (student # 51399111): optional awesome nickname 5
+ We call ourselves: The Wordlers!

# Acknowledgments
We received valuable support from various resources:

+ We used Hoogle to explore standard Haskell libraries, especially for string manipulation and I/O functions.
+ We were using https://hspec.github.io/ doc to create our tests and learn Hspec
+ The CPSC 312 Teaching Assistants provided feedback on game design and debugging... Eventually!

## Product Pitch
In a world filled with fast-paced, high-tech games, sometimes all you need is a simple, brain-teasing challenge to keep your mind sharp and entertained. That’s where our terminal-based version of Wordle comes in—a minimalist yet addictive word-guessing game that brings all the fun and strategy of the original to your fingertips. Our project isn’t just about recreating Wordle—it’s about delivering a clean, streamlined experience where the game mechanics shine through the power of Haskell’s functional programming, making every guess feel intuitive and every win that much more satisfying.

At its core, the game provides players with instant feedback on their guesses:

+ X for letters that aren't in the word,
+ O for letters that are in the wrong position,
+ and @ for letters that are spot-on.
This keeps players engaged and thinking critically until they crack the code. And when they do, they're met with a well-earned, triumphant message celebrating their victory.

The problem we’re solving is deceptively simple: how do we provide people with an engaging, mentally stimulating game that they can easily pick up and play—without the need for fancy graphics or complicated rules? We’re tackling the problem of boredom head-on by offering an experience that’s light on setup but heavy on satisfaction. And since this is built entirely in Haskell, we're leveraging its language-specific strengths to create clear, efficient game logic that can be expanded, tweaked, and optimized easily. The functional programming paradigm allows us to focus on what really matters: creating an enjoyable and seamless gameplay experience that will keep users coming back for more.

This is more than just a game—it’s a problem-solving exercise disguised as entertainment, and it’s built for anyone looking to challenge their brain while having a little fun. Whether you’re a seasoned word-game enthusiast or someone looking to kill a few minutes, this project has something for you.

# Minimal Viable Project (MVP)
We aims to create a fully functional version of the core Wordle gameplay loop, where users can guess a hidden word, receive instant feedback, and either win or lose based on their guesses. We’ll focus on three key areas:

1. Reading and processing user input: This will allow players to interact with the game naturally through a terminal interface.
2. Implementing the feedback system: Each letter in the player’s guess will be compared to the hidden word, generating feedback that is both clear and accurate using pattern matching and recursion.
3. Basic game flow: The game will start with a user prompt, enter into the feedback loop, and end either with success (correct guess) or failure (max attempts reached).
This MVP sets the foundation for the complete game by ensuring that the core functionality—what makes the game fun and addictive—is rock solid.

## Building on Haskell's Strengths:
The MVP will take full advantage of Haskell’s powerful pattern matching to efficiently compare guesses with the hidden word. This allows us to create clear, concise, and expressive game logic. We will also be using recursive function to ensure that the game loop flows smoothly and feels natural without unnecessary complexity.

We also plan to harness Haskell’s IO monad to handle user input/output cleanly, ensuring seamless interaction through the command line. This gives us a deeper understanding of how Haskell manages state and effects, which is critical for creating interactive programs!

## Learning New Language Elements:
While we’re already familiar with basic recursion and pattern matching, handling side effects (like user input/output) in Haskell is a new challenge for us. The MVP will explore how to elegantly manage game state and feedback in a functional way, without relying on the mutable state seen in other languages. We’ll dive deeper into monads, which opens up learning opportunities to better understand how Haskell separates pure functions from actions like reading input.

By focusing on these language features, we’re ensuring that our MVP isn’t just a stepping stone to the final game—it’s a meaningful project that highlights Haskell's capabilities while giving us a deeper appreciation of its functional programming principles!

## Importance of this MVP: 
This MVP is designed to tackle the most critical aspects of the game while giving us room to grow and refine the experience. It’s the foundation on which we’ll build a more polished, feature-rich version of Wordle. The minimal viable product will be functional, fun, and demonstrate Haskell’s strengths to create interactive games!

# Proof of Concept (** TODO **)
Our proof-of-concept brings to life the core of what makes Wordle-like games so addictively fun: the feedback loop. This feedback mechanism is the beating heart of the project—it takes the player’s guess, compares it to the hidden word, and delivers instant feedback in a clear and playful way. Every interaction hinges on this system, so it’s absolutely essential to get it right. And we have.

## Key Focus:
+ **Accepting player input**: We capture the user’s guess directly from the terminal, which adds an old-school, nostalgic feel to the game.
+ **Comparing the guess to the hidden word**: Each letter of the guess is meticulously compared to the hidden word.
+ **Delivering feedback**: Each letter is marked with:
  + X for letters that don’t exist in the word at all,
  + O for letters that are in the word but misplaced, and
  + @ for letters that are in the right spot.

## How to Test and Run the Proof of Concept: (** TODO **)
**TODO**
Testing the proof-of-concept is simple and fun! Here’s how to try it out:
1. Clone the repository from GitHub using HTTPS: git clone https://github.students.cs.ubc.ca/parsaz00/cpsc-312-project.git
2. Open a terminal and navigate to the project directory, it will be called cpsc-312-project
3. Compile and run the code with the following commands:
`stack clean`
`stack build`
`stack exec wordle-game-exe`
5. Enter a word guess when prompted, and see the instant feedback—watch the letters get marked as X, O, or @! If your input is shorter than the length of the correct answer, or wronger, the program will tell you and you will have to play again!
+ The code behind this feedback system can be found here. With this interactive proof-of-concept, you’ll immediately see how the core game works, and how it responds to your input.
6. To run the test code do, run `stack test`

## Links to the critical pieces of the code (** TODO **)
app/Main.hs

This is where we create our command line interface wordle game. 

Link 1: https://github.students.cs.ubc.ca/parsaz00/cpsc-312-project/blob/01f24c8d7c6c8453c62e886a0094c5a42ae94196/haskell/app/Main.hs#L3

System.IO is handling input output, GameLogic is where we actually designed the logic that checks guesses against the target word

Link 2: https://github.students.cs.ubc.ca/parsaz00/cpsc-312-project/blob/01f24c8d7c6c8453c62e886a0094c5a42ae94196/haskell/app/Main.hs#L8

This function will run one round of of the game. It asks a user for their guess and stores it. The checkGuess function is then called to generate feedback. The feedback is then printed back out to the user. If the feedback is all @ symbols, we congratulate the player for getting the question right. 

Link 3: https://github.students.cs.ubc.ca/parsaz00/cpsc-312-project/blob/01f24c8d7c6c8453c62e886a0094c5a42ae94196/haskell/app/Main.hs#L19

Here we hardcode the target word. Then we run the game. After the round ends, the user can decide (y/n) if they want to play again. If they do, the function recurisively calls itself to restart the game (with the same word for now). If the user chooses n then the game ends and we give them thanks for playing our game. 


src/GameLogic.hs

Link 1: https://github.students.cs.ubc.ca/parsaz00/cpsc-312-project/blob/01f24c8d7c6c8453c62e886a0094c5a42ae94196/haskell/src/GameLogic.hs#L6

This is the core logic for our POC. First we check the length of the guess against the length of the target. If it is too [short](https://github.students.cs.ubc.ca/parsaz00/cpsc-312-project/blob/01f24c8d7c6c8453c62e886a0094c5a42ae94196/haskell/src/GameLogic.hs#L8) or [too](https://github.students.cs.ubc.ca/parsaz00/cpsc-312-project/blob/01f24c8d7c6c8453c62e886a0094c5a42ae94196/haskell/src/GameLogic.hs#L9) long, we let the user know. 

If the guess is the same length as the target, we use the map function and zip function to compare each letter in the target against each letter in the guess with our helper funciton [checkLetter](https://github.students.cs.ubc.ca/parsaz00/cpsc-312-project/blob/01f24c8d7c6c8453c62e886a0094c5a42ae94196/haskell/src/GameLogic.hs#L12) . We especially take advantage of the zip function, which we discovered; it allows us to combine the target and guess strings into a list of tuples, where each tuple contains a pair of corresponding letters from the two strings. Then we can compare them using map and checkLetter, and generate the feedback. 

test/Spec.hs

We use the Hspec testing framework to define and run our tests. 

The [describe](https://github.students.cs.ubc.ca/parsaz00/cpsc-312-project/blob/01f24c8d7c6c8453c62e886a0094c5a42ae94196/haskell/test/Spec.hs#L9) block groups tests that are all related to our Worldle guess checking logic. 
Each test case is defined using the **it** function and it shows the individual behaviour we want to test. 

One of the reasons we chose Hspec is because of the syntax (such as describe and it) which makes our test code easy to read. We also wanted to focus on behaviour driven style tests as this is just a POC. 

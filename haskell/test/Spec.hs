module Main where

import GameLogic (checkGuess)
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "Wordle Guess Checker" $ do
    it "Returns '@' for correct letters in correct positions" $ do
      checkGuess "haskell" "haskell" `shouldBe` replicate 7 '@'

    it "Returns 'X' for incorrect letters" $ do
      checkGuess "haskell" "zzzzzzz" `shouldBe` replicate 7 'X'

    it "Returns 'O' for correct letters in wrong positions" $ do
      checkGuess "haskell" "kashlel" `shouldBe` "O@@OOO@"
    
    it "Returns correct response when guess length is shorted than target" $ do
      checkGuess "haskell" "a" `shouldBe` "guess is too short"
    
    it "Returns correct response when guess length is longer than target" $ do
      checkGuess "haskell" "aasdfadsfasdfasdfasdf" `shouldBe` "guess is too long"
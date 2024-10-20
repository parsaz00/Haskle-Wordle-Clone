module Spec where

import Test.Hspec
import Main (checkGuess)

main :: IO ()
main = hspec $ do
  describe "Wordle Guess Checker" $ do
    it "Returns '@' for correct letters in correct positions" $ do
      checkGuess "haskell" "haskell" `shouldBe` replicate 7 '@'

    it "Returns 'X' for incorrect letters" $ do
      checkGuess "haskell" "zzzzzzz" `shouldBe` replicate 7 'X'

    it "Returns 'O' for correct letters in wrong positions" $ do
      checkGuess "haskell" "kashlel" `shouldBe` "O@O@@O@"

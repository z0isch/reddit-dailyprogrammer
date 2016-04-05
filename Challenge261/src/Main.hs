module Main where

import           Easy
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Easy" $ do
    it "Test 1" $ isMagicSquare test1 `shouldBe` True
    it "Test 2" $ isMagicSquare test2 `shouldBe` True
    it "Test 3" $ isMagicSquare test3 `shouldBe` False
    it "Test 4" $ isMagicSquare test4 `shouldBe` False
    it "Test 5" $ isMagicSquare (fillInLastRow test5) `shouldBe` True
    it "Test 6" $ isMagicSquare (fillInLastRow test6) `shouldBe` False

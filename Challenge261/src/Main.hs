module Main where

import           Data.Maybe
import qualified Easy         as E
import qualified Intermediate as I
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Easy" $ do
    it "Test 1" $ E.isMagicSquare (E.parseMagicSquare E.test1) `shouldBe` True
    it "Test 2" $ E.isMagicSquare (E.parseMagicSquare E.test2) `shouldBe` True
    it "Test 3" $ E.isMagicSquare (E.parseMagicSquare E.test3) `shouldBe` False
    it "Test 4" $ E.isMagicSquare (E.parseMagicSquare E.test4) `shouldBe` False
    it "Test 5" $ E.isMagicSquare (E.parseMagicSquare (E.fillInLastRow E.test5)) `shouldBe` True
    it "Test 6" $ E.isMagicSquare (E.parseMagicSquare (E.fillInLastRow E.test6)) `shouldBe` False
  describe "Intermediate" $ do
    it "Test 1" $ isJust (I.findMagicSquare I.test1) `shouldBe` True
    it "Test 2" $ isJust (I.findMagicSquare I.test2) `shouldBe` True
    it "Test 3" $ isJust (I.findMagicSquare I.test3) `shouldBe` True
    it "Test 4" $ isJust (I.findMagicSquare I.test4) `shouldBe` True

module Main where

import qualified Easy          as E
import           Test.Hspec
import           Text.Trifecta

isString x (E.EasyString y) = x == y
isString _ _ = False

isNumber x (E.EasyNumber y) = x == y
isNumber _ _ = False

correctParse (Success True) = True
correctParse _ = False

main :: IO ()
main = hspec $ do
  describe "Easy" $ do
    it "Test 1" $ correctParse (isNumber 123 <$> E.parseTestData E.test1) `shouldBe` True
    it "Test 2" $ correctParse (isNumber 44.234 <$> E.parseTestData E.test2) `shouldBe` True
    it "Test 3" $ correctParse (isString "0x123N" <$> E.parseTestData E.test3) `shouldBe` True

module Main where

import qualified Easy          as E
import           Test.Hspec
import           Text.Trifecta

isString x (E.EasyString y) = x == y
isString _ _ = False

isArray x (E.EasyArray y) = x == y
isArray _ _ = False

correctParse (Success True) = True
correctParse _ = False

main :: IO ()
main = hspec $ do
  describe "Easy" $ do
    it "Test 1" $ correctParse (isArray [123] <$> E.parseTestData E.test1) `shouldBe` True
    it "Test 2" $ correctParse (isArray [44.234] <$> E.parseTestData E.test2) `shouldBe` True
    it "Test 3" $ correctParse (isString "0x123N" <$> E.parseTestData E.test3) `shouldBe` True
    it "Test 4" $ correctParse (isArray [3.23e5] <$> E.parseTestData E.test4) `shouldBe` True
    it "Test 5" $ correctParse (isArray [1293712938712938172938172391287319237192837329] <$> E.parseTestData E.test5) `shouldBe` True
    it "Test 6" $ correctParse (isArray [0.25] <$> E.parseTestData E.test6) `shouldBe` True
    it "Test 7" $ correctParse (isArray [123,234,345] <$> E.parseTestData E.test7) `shouldBe` True
    it "Test 8" $ correctParse (and . zipWith ($) [isArray [2015,4,4],isString "Challenge #",isArray [261],isString "Easy"] <$> E.mParseTestData E.test8) `shouldBe` True
    it "Test 9" $ correctParse (and . zipWith ($) [isArray [234.2],isString "234ggf 45",isArray [0.0],isString "number string number (0)"] <$> E.mParseTestData E.test9) `shouldBe` True

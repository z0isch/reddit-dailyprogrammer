module Main where

import           Data.Map     ((!))
import qualified Easy         as E
import qualified Intermediate as I
import           Test.Hspec

main :: IO ()
main = do
  let easyInput = ["122333444455555666666777777788888888","563881467447538846567288767728553786","https://www.reddit.com/r/dailyprogrammer","int main(int argc, char *argv[])"]
  rhymeDictionary <- I.pronouncingDictionary
  let matchingRhymes = I.rhymeWords I.isMatchingPhoneme rhymeDictionary "SOLUTION"
  let challenegeMatchingRhymes = I.rhymeWords I.challengeIsMatchingPhoneme rhymeDictionary "NOIR"
  hspec $ do
    describe "Easy" $ do
      it "Test 1" $ take 11 (show (E.shannonEntropy (easyInput !! 0))) `shouldBe` "2.794208683"
      it "Test 2" $ take 11 (show (E.shannonEntropy (easyInput !! 1))) `shouldBe` "2.794208683"
      it "Test 3" $ take 11 (show (E.shannonEntropy (easyInput !! 2))) `shouldBe` "4.056198332"
      it "Test 4" $ take 11 (show (E.shannonEntropy (easyInput !! 3))) `shouldBe` "3.866729296"
    describe "Intermediate" $ do
      it "Test 0" $ fst (matchingRhymes ! "ABSOLUTION") `shouldBe` 7
      it "Test 1" $ fst (matchingRhymes ! "DISSOLUTION") `shouldBe` 7
      it "Test 2" $ fst (matchingRhymes ! "ALEUTIAN") `shouldBe` 6
      it "Test 3" $ fst (matchingRhymes ! "ANDALUSIAN") `shouldBe` 6
      it "Test 4" $ fst (matchingRhymes ! "ZUPAN") `shouldBe` 2
      it "Test 5" $ fst (matchingRhymes ! "ZURKUHLEN") `shouldBe` 2
      it "Test 6" $ fst (matchingRhymes ! "ZWAHLEN") `shouldBe` 2
      it "Test 7" $ fst (matchingRhymes ! "ZYMAN") `shouldBe` 2
      it "Test 8" $ fst (challenegeMatchingRhymes ! "BOUDOIR") `shouldBe` 2
      it "Test 9" $ fst (challenegeMatchingRhymes ! "LOIRE") `shouldBe` 2
      it "Test 10" $ fst (challenegeMatchingRhymes ! "MOIR") `shouldBe` 2
      it "Test 11" $ fst (challenegeMatchingRhymes ! "SOIR") `shouldBe` 2

module Main where

import qualified Easy       as E
import           Test.Hspec

main :: IO ()
main = do
  let easyInput = ["122333444455555666666777777788888888","563881467447538846567288767728553786","https://www.reddit.com/r/dailyprogrammer","int main(int argc, char *argv[])"]
  hspec $ do
    describe "Easy" $ do
      it "Test 1" $ take 11 (show (E.shannonEntropy (easyInput !! 0))) `shouldBe` "2.794208683"
      it "Test 2" $ take 11 (show (E.shannonEntropy (easyInput !! 1))) `shouldBe` "2.794208683"
      it "Test 3" $ take 11 (show (E.shannonEntropy (easyInput !! 2))) `shouldBe` "4.056198332"
      it "Test 4" $ take 11 (show (E.shannonEntropy (easyInput !! 3))) `shouldBe` "3.866729296"

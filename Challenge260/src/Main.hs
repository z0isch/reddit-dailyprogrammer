module Main where

import           Easy
import           Hard ()

main :: IO ()
main = print $ scanl processCommand Closed input

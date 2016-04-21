module Easy where

import           Data.List

shannonEntropy :: (Floating a, Ord a1) => [a1] -> a
shannonEntropy s = (-1) * sum (map (\c ->  freq c * logBase 2 (freq c)) grouped)
  where
    grouped = group $ sort s
    l = genericLength s
    freq c = genericLength c / l

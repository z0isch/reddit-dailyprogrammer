
module Easy where

import           Data.List
import           Data.List.Split

sumNeeded :: Integral a => a -> a
sumNeeded sideLength = sideLength * (sideLength * sideLength + 1) `div` 2

isMagicSquare :: [[Integer]] -> Bool
isMagicSquare square = all (== sumNeeded sideLength) $
  map sum square
  ++ map sum (transpose square)
  ++ [sum $ diagonal square]
  ++ [sum $ diagonal $ map reverse square]
  where
    diagonal = zipWith (flip (!!)) [0..]
    sideLength = genericLength $ head square

parseMagicSquare :: [Integer] -> [[Integer]]
parseMagicSquare xs = chunksOf sideLength xs
  where
    sideLength = floor $ sqrt $ genericLength xs

fillInLastRow :: [Integer] -> [Integer]
fillInLastRow xs = xs ++ map ((sumNeeded sideLength - ) . sum) (transpose square)
  where
    square = chunksOf (fromIntegral sideLength) xs
    sideLength = floor $ sqrt nextSquare
    nextSquare = head $ dropWhile (< genericLength xs) $ map (^2) [1..]

test1 = [8, 1, 6, 3, 5, 7, 4, 9, 2]
test2 = [2, 7, 6, 9, 5, 1, 4, 3, 8]
test3 = [3, 5, 7, 8, 1, 6, 4, 9, 2]
test4 = [8, 1, 6, 7, 5, 3, 4, 9, 2]
test5 = [8, 1, 6, 3, 5, 7]
test6 = [3, 5, 7, 8, 1, 6]

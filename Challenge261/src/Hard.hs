module Hard where

import           Control.Monad
import           Data.List
import           Data.List.Split
import qualified Easy            as E
import qualified Intermediate    as I
import           Text.Trifecta


type Tile = (Integer,Integer)

t0 = concatMap (\(x,y) -> [x,y]) . (!! 0) <$> testInput
t1 = (!! 0) <$> testInput
t4 = concatMap (\(x,y) -> [x,y]) . (!! 5) <$> testInput

t xs = filter (\ys -> sum ys == 34) $ map fst xs

g (x1,x2) ys = (,) (x1 ++ [x2]) <$> ys \\ (x1 ++ [x2])

f xs = do
  x <- (,) [] <$> xs
  foldM g x [xs,xs,xs,xs]

testInput :: IO [[Tile]]
testInput = do
  test <- parseFromFile parseFile ".\\hard\\tests.txt"
  case test of
    Nothing -> return []
    Just ts -> return ts

parseLine :: Parser Tile
parseLine = (,) <$> I.integerParser <* char ' ' <*> I.integerParser <* I.endOfLine

parseInput :: Parser [Tile]
parseInput = I.skipLine *> some parseLine <* skipOptional I.skipLine

parseFile :: Parser [[Tile]]
parseFile = some parseInput <* eof

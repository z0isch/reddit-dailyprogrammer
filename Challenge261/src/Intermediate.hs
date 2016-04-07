module Intermediate where

import           Data.List
import qualified Easy          as E
import           Text.Trifecta

findMagicSquare :: [[Integer]] -> Maybe ([[Integer]],Bool)
findMagicSquare square = lastMay $ dropWhileEnd (\(_,x) -> not x) (squares square)

squares :: [[Integer]] -> [([[Integer]], Bool)]
squares sq = map (\x -> (x,E.isMagicSquare x)) $ permutations sq

lastMay :: [a] -> Maybe a
lastMay xs
  | null xs = Nothing
  | otherwise = Just (last xs)

testSquares :: IO [[[Integer]]]
testSquares = do
  test <- parseFromFile parseFile ".\\intermediate\\tests.txt"
  case test of
    Nothing -> return [[[]]]
    Just ts -> return ts

endOfLine :: Parser Char
endOfLine = choice [newline,char '\r' *> newline]

skipLine :: Parser String
skipLine = manyTill anyChar endOfLine

integerParser :: Parser Integer
integerParser = read <$> some digit

parseSquareLine :: Parser [Integer]
parseSquareLine = integerParser `sepBy1` char ' ' <* endOfLine

parseSquare :: Parser [[Integer]]
parseSquare = skipLine *> some parseSquareLine <* skipOptional skipLine

parseFile :: Parser [[[Integer]]]
parseFile = some parseSquare <* eof

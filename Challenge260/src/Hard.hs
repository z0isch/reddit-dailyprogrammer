{-# LANGUAGE FlexibleContexts #-}

module Hard where

import           Text.Trifecta

data Tile = TileFood | TileEmpty | TileSnake | TileWall | TilePit
  deriving (Show, Eq)

tiles :: String -> IO [[Tile]]
tiles n = do
  test <- parseFromFile parseFile (".\\hard\\" ++ n)
  case test of
    Nothing -> return [[]]
    Just ts -> return ts

endOfLine :: Parser Char
endOfLine = choice [newline,char '\r' *> char '\n']

skipLine :: Parser String
skipLine = manyTill anyChar endOfLine

parseFile :: Parser [[Tile]]
parseFile = do
  _ <- skipLine
  pTiles <- some $ some parseTile <* endOfLine
  _ <- skipLine
  let top = [replicate (length (head pTiles)) TileWall]
  let bottom = [replicate (length (last pTiles)) TileWall]
  return $ top ++ pTiles ++ bottom

parseTile :: Parser Tile
parseTile = p <$> oneOf ['|', ' ', 'O', 's', '*']
  where
    p '|' = TileWall
    p ' ' = TileEmpty
    p 's' = TileSnake
    p 'O' = TilePit
    p '*' = TileFood
    p _ = error ""

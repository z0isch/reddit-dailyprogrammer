{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified          Text.Parsec as P
import qualified Data.List as L
import Control.Applicative
import Data.Maybe
import Control.Monad

data Suit = Bamboo | Circle | Character
  deriving (Show,Eq, Read, Ord)

data Tile = Tile Suit Integer
  deriving (Show,Eq)

data GroupType = Pair | Set | Quad | Sequence
  deriving (Show,Eq)

data HandGroup = HandGroup GroupType [Tile]
  deriving (Show,Eq)

isPair :: Tile -> Tile -> Bool
isPair = (==)
maybePair :: Tile -> Tile -> Maybe HandGroup
maybePair t1 t2
  | isPair t1 t2 = Just $ HandGroup Pair [t1,t2]
  | otherwise = Nothing

isSet :: Tile -> Tile -> Tile -> Bool
isSet t1 t2 t3 = t1 == t2 && t2 == t3
maybeSet :: Tile -> Tile -> Tile -> Maybe HandGroup
maybeSet t1 t2 t3
  | isSet t1 t2 t3 = Just $ HandGroup Set [t1,t2,t3]
  | otherwise = Nothing

isQuad :: Tile -> Tile -> Tile -> Tile -> Bool
isQuad t1 t2 t3 t4 = isSet t1 t2 t3 && t1 == t4
maybeQuad :: Tile -> Tile -> Tile -> Tile -> Maybe HandGroup
maybeQuad t1 t2 t3 t4
  | isQuad t1 t2 t3 t4 = Just $ HandGroup Quad [t1,t2,t3,t4]
  | otherwise = Nothing

isSequence :: Tile -> Tile -> Tile -> Bool
isSequence (Tile s1 i1) (Tile s2 i2) (Tile s3 i3)
  | s1 == s2 && s2 == s3 = o1 + 1 == o2 && o2 + 1 == o3
  | otherwise = False
  where
    [o1,o2,o3] = L.sort [i1,i2,i3]
maybeSequence :: Tile -> Tile -> Tile -> Maybe HandGroup
maybeSequence t1 t2 t3
  | isSequence t1 t2 t3 = Just $ HandGroup Sequence [t1,t2,t3]
  | otherwise = Nothing

suitGroupPred :: Tile -> Tile -> Bool
suitGroupPred (Tile s1 i1) (Tile s2 i2) = s1 == s2

step :: ([HandGroup],[Tile]) -> [Maybe ([HandGroup],[Tile])]
step (hg,[]) = [Just (hg,[])]
step (_,[t]) = [Nothing]
step (hg,[t1,t2]) = [mkList (hg,[]) $ maybePair t1 t2]
step (hg,[t1,t2,t3]) = [mkList (hg,[]) $ maybeSet t1 t2 t3 <|> maybeSequence t1 t2 t3]
step (hg,t1:t2:t3:t4:ts) =
  [ mkList (hg,t3:t4:ts) $ maybePair t1 t2
  , mkList (hg,t4:ts) $ maybeSet t1 t2 t3 <|> maybeSequence t1 t2 t3
  , mkList (hg,ts) $ maybeQuad t1 t2 t3 t4
  ]

mkList :: ([HandGroup],[Tile]) -> Maybe HandGroup -> Maybe ([HandGroup],[Tile])
mkList (hg,ts) m = liftA (flip (,) ts) $ (++) <$> Just hg <*> fmap pure m

createHandGroups :: ([HandGroup],[Tile]) -> [[HandGroup]]
createHandGroups (hgs,[]) = [hgs]
createHandGroups s@(hgs,ts) = concatMap createHandGroups $ catMaybes $ step s

winningHand :: [HandGroup] -> Bool
winningHand [] = False
winningHand hgs
  | Quad `elem` groups = numPairs == 1 && length tiles == 14 + numQuads
  | otherwise = length tiles == 14
  where
    groups = map (\(HandGroup g _) -> g) hgs
    numPairs = length $ L.elemIndices Pair groups
    numQuads= length $ L.elemIndices Quad groups
    tiles = concatMap (\(HandGroup _ ts) -> ts) hgs

main :: IO ()
main = do
  test1 <- P.parse parseFile "" <$> readFile "test1.txt"
  case test1 of
    Left err -> print err
    Right ts -> print $
      any winningHand $
      concatMap (createHandGroups . (\a-> ([],a)) . concat) $
      sequence $
      map L.permutations $ L.groupBy suitGroupPred $ L.sortOn (\(Tile s i) -> s) ts

parseFile :: P.Stream s m Char => P.ParsecT s u m [Tile]
parseFile = do
  t <- parseInteger <* P.endOfLine
  tiles <- P.many1 parseTile
  P.eof
  return tiles

parseTile :: P.Stream s m Char => P.ParsecT s u m Tile
parseTile = do
  suit <- read <$> P.many1 P.letter <* P.char ','
  num <- parseInteger
  P.endOfLine
  return (Tile suit num)

parseInteger :: P.Stream s m Char => P.ParsecT s u m Integer
parseInteger = read <$> P.many1 P.digit

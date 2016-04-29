module Challenge263.Intermediate where

import           Data.List
import           Data.Map            (Map, (!))
import qualified Data.Map            as Map
import           Data.Maybe
import qualified Text.Read           as R
import           Text.Trifecta
import           Text.Trifecta.Delta

type Stress = Integer

data PhonemeType = Vowel Stress | Stop | Affricate | Fricative | Aspirate | Liquid | Semivowel | Nasal
  deriving (Show, Eq, Ord)

data Phoneme = Phoneme String PhonemeType
    deriving (Show, Eq, Ord)

isVowel (Phoneme _ (Vowel _)) = True
isVowel _ = False

challengeIsMatchingPhoneme :: Phoneme -> Phoneme -> Bool
challengeIsMatchingPhoneme (Phoneme a1 (Vowel s1)) (Phoneme a2 (Vowel s2)) = a1 == a2
challengeIsMatchingPhoneme p1 p2 = p1 == p2

isMatchingPhoneme :: Phoneme -> Phoneme -> Bool
isMatchingPhoneme = (==)

numMatchingPhonemes :: (Phoneme -> Phoneme -> Bool) -> [Phoneme] -> [Phoneme] -> Integer
numMatchingPhonemes eq x y = genericLength $ takeWhile (== True) $ zipWith eq (reverse x) (reverse y)

rhymeWords :: (Phoneme -> Phoneme -> Bool) -> Map String [Phoneme] -> String -> Map String (Integer,[Phoneme])
rhymeWords eq d s = Map.map (\a -> (numMatchingPhonemes eq pS a, a)) $
  Map.filter (\a -> all (== True) $ zipWith eq (lastSyllable a) (lastSyllable pS)) d
  where pS = d ! s

lastSyllable :: [Phoneme] -> [Phoneme]
lastSyllable = reverse . takeWhileInclusive (not . isVowel) . reverse

takeWhileInclusive :: (t -> Bool) -> [t] -> [t]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []

pronouncingDictionary :: String -> IO (Map String [Phoneme])
pronouncingDictionary s =  do
  test <- parseFromFile parseDictionaryFile s
  case test of
    Nothing -> return Map.empty
    Just ts -> return ts

parseDictionaryFile :: Parser (Map String [Phoneme])
parseDictionaryFile = Map.fromList <$> (some commentLine *> manyTill dictionaryLine (try eof))

dictionaryLine :: Parser (String,[Phoneme])
dictionaryLine = f <$> manyTill anyChar (try (string "  ")) <*> some alphaNum `sepBy1` char ' ' <* endOfLine
  where
    f s p = (s, map phoneme p)

commentLine :: Parser String
commentLine = string ";;;" *> manyTill anyChar endOfLine

endOfLine :: Parser Char
endOfLine = choice [newline,char '\r' *> newline]

phoneme :: String -> Phoneme
phoneme s
  | isJust (R.readMaybe [last s] :: Maybe Integer) = Phoneme (init s) (Vowel $ read [last s])
  | otherwise = nonVowelPhoneme s
  where
  nonVowelPhoneme "B" = Phoneme "B" Stop
  nonVowelPhoneme "CH" = Phoneme "CH" Affricate
  nonVowelPhoneme "D" = Phoneme "D" Stop
  nonVowelPhoneme "DH" = Phoneme "DH" Fricative
  nonVowelPhoneme "F" = Phoneme "F" Fricative
  nonVowelPhoneme "G" = Phoneme "G" Stop
  nonVowelPhoneme "HH" = Phoneme "HH" Aspirate
  nonVowelPhoneme "JH" = Phoneme "JH" Affricate
  nonVowelPhoneme "K" = Phoneme "K" Stop
  nonVowelPhoneme "L" = Phoneme "L" Liquid
  nonVowelPhoneme "M" = Phoneme "M" Nasal
  nonVowelPhoneme "N" = Phoneme "N" Nasal
  nonVowelPhoneme "NG" = Phoneme "NG" Nasal
  nonVowelPhoneme "P" = Phoneme "P" Stop
  nonVowelPhoneme "R" = Phoneme "R" Liquid
  nonVowelPhoneme "S" = Phoneme "S" Fricative
  nonVowelPhoneme "SH" = Phoneme "SH" Fricative
  nonVowelPhoneme "T" = Phoneme "T" Stop
  nonVowelPhoneme "TH" = Phoneme "TH" Fricative
  nonVowelPhoneme "V" = Phoneme "V" Fricative
  nonVowelPhoneme "W" = Phoneme "W" Semivowel
  nonVowelPhoneme "Y" = Phoneme "Y" Semivowel
  nonVowelPhoneme "Z" = Phoneme "Z" Fricative
  nonVowelPhoneme "ZH" = Phoneme "ZH" Fricative

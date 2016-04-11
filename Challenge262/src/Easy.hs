module Easy where

import           Control.Monad
import           Data.List
import           Data.Maybe
import           Text.Trifecta
import           Text.Trifecta.Delta

data EasyParsed = EasyArray [Double] | EasyString String
  deriving (Show)

test1 = "123"
test2 = "44.234"
test3 = "0x123N"
test4 = "3.23e5"
test5 = "1293712938712938172938172391287319237192837329"
test6 = ".25"
test7 = "123 234 345"
test8 = "2015 4 4`Challenge #`261`Easy"
test9 = "234.2`234ggf 45`00`number string number (0)"

parseTestData t = parseString easyParse (Columns (genericLength t) 0) t

mParseTestData t = parseString mEasyParse (Columns (genericLength t) 0) t

mEasyParse = easyParse `sepBy1` char '`'

easyParse = choice [try arrayParser, stringParser]

arrayParser = EasyArray <$> numberParser `sepBy1` char ' '

stringParser = EasyString <$> some (notChar '`')

numberParser = p <$> option "0" (some digit) <*> optional ((++) <$> numPart '.' <*> option "" (numPart 'e')) <* notFollowedBy (noneOf [' ','`'])
  where
    p x (Just y) = read (x ++ y)
    p x Nothing = read x
    numPart c = (:) <$> char c <*> some digit

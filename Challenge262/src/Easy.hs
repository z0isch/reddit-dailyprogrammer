module Easy where

import           Data.List
import           Data.Maybe
import           Text.Trifecta
import           Text.Trifecta.Delta

data EasyParsed = EasyString String | EasyNumber Double
  deriving (Show)

test1 = "123"
test2 = "44.234"
test3 = "0x123N"

parseTestData t = parseString easyParse (Columns (genericLength t) 0) t

easyParse = choice [try numberParser, stringParser]

stringParser = EasyString <$> some anyChar

numberParser = p <$> some digit <*> choice [end, decimalPart]
  where
    p x y = EasyNumber $ read (x ++ y)
    end = const ".0" <$> eof
    decimalPart = (:) <$> char '.' <*> some digit

module Hard where

import           Challenge263.Intermediate (Phoneme)
import qualified Challenge263.Intermediate as C263
import           Data.Char
import           Data.List
import           Data.Map                  (Map, (!))
import qualified Data.Map                  as Map

dict :: IO (Map String [Phoneme])
dict = C263.pronouncingDictionary "..\\Challenge263\\intermediate\\pronouncing-dictionary.txt"


getLines d = map snd .
             sortOn fst .
             concatMap (\(l,(_,xs)) -> map (flip (,) l) xs) .
             zip ['a'..] .
             sortOn (\(ps,is) -> minimum is) .
             Map.toList . Map.fromListWith (++) .
             zipWith (\i ps -> (ps,[i])) [0..] .
             map (getLastPhoneme d . getLastWord)

getLastPhoneme :: Map String [Phoneme] -> String -> [Phoneme]
getLastPhoneme d w = C263.lastSyllable $ d ! w

getLastWord :: String -> String
getLastWord = filter isLetter . map toUpper . last . words

input1 = ["A bather whose clothing was strewed","By winds that left her quite nude","Saw a man come along","And unless we are wrong","You expected this line to be lewd."]
input2 = ["There once was a young lady named bright","Whose speed was much faster than light","She set out one day","In a relative way","And returned on the previous night."]
input3 =  ["Once upon a midnight dreary, while I pondered, weak and weary,","  Over many a quaint and curious volume of forgotten lore—",  "While I nodded, nearly napping, suddenly there came a tapping,","  As of some one gently rapping, rapping at my chamber door.", "\"'Tis some visiter,\" I muttered, \"tapping at my chamber door—", "Only this and nothing more.\""]
input4 = ["Brothers, who when the sirens roar","From office, shop and factory pour","'Neath evening sky;","By cops directed to the fug","Of talkie-houses for a drug,","Or down canals to find a hug"]
input5 = ["Two roads diverged in a yellow wood,","And sorry I could not travel both","And be one traveler, long I stood","And looked down one as far as I could","To where it bent in the undergrowth;"]

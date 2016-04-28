module Intermediate where
import           Data.Bits
import           Data.List
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M

type Gossip = Integer
type Stop = Integer
type Route = [Stop]

data Driver = Driver Integer Route Gossip
  deriving (Show)

getGossip :: Driver -> Gossip
getGossip (Driver _ _ g) = g
setGossip :: Gossip -> Driver -> Driver
setGossip g (Driver n r _) = Driver n r g

makeDriver :: Integer -> Route -> Driver
makeDriver x r = Driver x (concat $ repeat r) (bit (fromInteger x))

makeDrivers :: [[Integer]] -> [Driver]
makeDrivers = zipWith ($) (map makeDriver [0..])

knowsAllGossip :: [Driver] -> Bool
knowsAllGossip drs = all (== genericLength drs) $ map (popCount . getGossip) drs

step :: [Driver] -> [Driver]
step drs = concatMap snd $ M.toList $ M.mapWithKey (\s ds -> map (setGossip (gossipMap ! s)) ds) driverMap
  where
    driverMap :: Map Stop [Driver]
    driverMap = M.fromListWith (++) $ map (\(Driver n r g) -> (head r, [Driver n (tail r) g])) drs
    gossipMap :: Map Stop Gossip
    gossipMap = M.map (foldl (.|.) 0 . map getGossip) driverMap

challengeStep :: [Driver] -> [Driver]
challengeStep drs = zipWith gossipSharer (sortedDrivers drs) (sortedDrivers (step drs))
  where
  gossipSharer (Driver n1 r1 g1) d2@(Driver _ _ g2)
    | g1 == g2 = d2
    | otherwise = Driver n1 r1 g2
  sortedDrivers = sortOn (\(Driver x _ _) -> x)

solve :: ([Driver] -> [Driver]) -> [Driver] -> Int
solve s = length . takeWhile (== False) . map knowsAllGossip . take 480 . iterate s

solveRegular :: [Driver] -> Int
solveRegular = solve step
solveChallenge :: [Driver] -> Int
solveChallenge = solve challengeStep

testDrivers1 = makeDrivers [[3,1,2,3],[3,2,3,1],[4,2,3,4,5]]
testDrivers2 = makeDrivers [[2,1,2],[5,2,8]]
testDrivers3 = makeDrivers [[7,11,2,2,4,8,2,2],[3,0,11,8],[5,11,8,10,3,11],[5,9,2,5,0,3],[7,4,8,2,8,1,0,5],[3,6,8,9],[4,2,11,3,3]]
testDrivers4 = makeDrivers [[12,23,15,2,8,20,21,3,23,3,27,20,0],[21,14,8,20,10,0,23,3,24,23,0,19,14,12,10,9,12,12,11,6,27,5],[8,18,27,10,11,22,29,23,14],[13,7,14,1,9,14,16,12,0,10,13,19,16,17],[24,25,21,4,6,19,1,3,26,11,22,28,14,14,27,7,20,8,7,4,1,8,10,18,21],[13,20,26,22,6,5,6,23,26,2,21,16,26,24],[6,7,17,2,22,23,21],[23,14,22,28,10,23,7,21,3,20,24,23,8,8,21,13,15,6,9,17,27,17,13,14],[23,13,1,15,5,16,7,26,22,29,17,3,14,16,16,18,6,10,3,14,10,17,27,25],[25,28,5,21,8,10,27,21,23,28,7,20,6,6,9,29,27,26,24,3,12,10,21,10,12,17],[26,22,26,13,10,19,3,15,2,3,25,29,25,19,19,24,1,26,22,10,17,19,28,11,22,2,13],[8,4,25,15,20,9,11,3,19],[24,29,4,17,2,0,8,19,11,28,13,4,16,5,15,25,16,5,6,1,0,19,7,4,6],[16,25,15,17,20,27,1,11,1,18,14,23,27,25,26,17,1]]

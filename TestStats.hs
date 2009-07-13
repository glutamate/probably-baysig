module Main where

--import Math.Statistics
import Math.Statistics.Fusion
import Data.Array.Vector
import FoldingStats
import ChunkedArray

runStatCU :: (UA b) => Fold b c -> Chunked chsz b -> c
runStatCU (F f x c _) = c . (foldlCU f x) 
--runStatCU (F f x c _) arr = let acc = foldlCU f x arr in acc `seq` c acc

--main = print $ mean [1..1e7]
--main = print . mean $ myArr
main = print $ runStat meanF myLst
--main = print . runStatU meanF $ myArr
--main = print . runStatCU meanF $ myChLst

myArr :: UArr Double
--myArr = mapU toDbl $ enumFromToU (1::Int) 10000000
myArr = enumFromToFracU 1 10000000

myLst :: [Double]
--myLst = map toDbl $ [1..10000000]
myLst = [1..10000000]


myChLst = mapCU toDbl $ enumFromToCU D10000 1 10000000

toDbl :: Int -> Double
toDbl = realToFrac

module Main where

--import Math.Statistics
import Math.Statistics.Fusion
import Data.Array.Vector
import FoldingStats

--main = print $ mean [1..1e7]
--main = print . mean $ myArr
main = print $ runStat meanF myLst
--main = print . runStatU meanF $ myArr

myArr :: UArr Double
myArr = enumFromToFracU 1 1e7

myLst :: [Double]
myLst = [1..1e7]

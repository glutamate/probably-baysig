--stolen from kmeans!

module Math.Probably.KMedoids
    where
import Data.List (transpose, sort, groupBy, minimumBy, nub)
import Data.Ord (comparing)
--import Numeric.LinearAlgebra
import qualified Data.Map.Strict as Map

import Numeric.LinearAlgebra
import Control.Monad
import Debug.Trace
import Data.Random

type DistMap = ([Int] , (Int -> Int -> Double))


kmedoids :: Int -> Int -> DistMap -> RVar (Map.Map Int [Int])
kmedoids nclust iters dist = do
  inits <- kmedoidInit nclust dist
  return $ kmedoidIter dist inits iters

kmedoidsMany :: Int -> Int -> Int -> DistMap -> RVar [Int]
kmedoidsMany nclust iters tries dist = do
  ress <- forM [1..tries] $ \i -> do
     clusters <- kmedoids nclust iters dist
     let cost = totalCost dist clusters
     return $ trace ("KMed try = "++show i++ " nclus="++show nclust++" cost= "++show cost) (clusters, cost)
  return $ Map.keys $ fst $ minimumBy (comparing snd) ress


kmedoidInit :: Int -> DistMap -> RVar [Int]
kmedoidInit nclust dist = fmap (sort . take nclust) $ shuffle $  fst dist


kmedoidIter :: DistMap -> [Int] -> Int -> Map.Map Int [Int]
kmedoidIter dists meds 0 = assignToClosestMedoid dists meds
kmedoidIter dists meds0 iter = next where
  clusters = assignToClosestMedoid dists meds0
  meds1 = sort $ nub $ map (\(_, pts)-> reassignMedoid dists pts) $ Map.toList clusters
  next = {-trace (show (meds0, totalCost dists clusters))
         $ -} if meds1 == meds0
                 then assignToClosestMedoid dists meds1
                 else kmedoidIter dists meds1 (iter-1)

totalCost :: DistMap -> Map.Map Int [Int] -> Double
totalCost dists  = sum . map ccost . Map.toList where
  ccost (med, pts) = sum $ map (getDist dists med) pts


getDist :: DistMap -> Int -> Int -> Double
getDist (_,f) from to
  | from == to = 0
  | otherwise = f from to

assignToClosestMedoid :: DistMap -> [Int] -> Map.Map Int [Int]
assignToClosestMedoid dists medoids = clusmap where
  assignmap = Map.fromList $ map (\k -> (k, findNearestMedoid dists medoids k)) $ fst dists
  clusmap = Map.fromList $ map (\med -> (med, keysWhereValIs assignmap med)) medoids

keysWhereValIs :: Eq a => Map.Map k a -> a -> [k]
keysWhereValIs mp val = Map.keys $ Map.filter (==val) mp


findNearestMedoid :: DistMap -> [Int] -> Int -> Int
findNearestMedoid dists meds k
  = fst $ minimumBy (comparing snd) $ map (\med-> (med, getDist dists k med)) meds

calcCost :: DistMap -> Int -> [Int] -> Double
calcCost dists med  = sum . map (getDist dists med)

reassignMedoid :: DistMap -> [Int] -> Int
reassignMedoid dists points
  = fst $ minimumBy (comparing snd) $ map (\pt-> (pt, calcCost dists pt points) ) points

--stolen from kmeans!

module Math.Probably.KMedoids
    where
import Data.List (transpose, sort, groupBy, minimumBy)
import Data.Function (on)
import Data.Ord (comparing)
--import Numeric.LinearAlgebra
import qualified Data.Map.Strict as Map

import Numeric.LinearAlgebra
import Math.Probably.Types
import Math.Probably.Sampler

type DistMap = Map.Map (Int, Int) Double


kmedoids :: Int -> Int -> DistMap -> Prob [Int]
kmedoids nclust iters dists = do
  inits <- nDistinctOf nclust $ allKeys dists
  return $ kmedoidIter dists inits iters

kmedoidIter :: DistMap -> [Int] -> Int -> [Int]
kmedoidIter _ meds 0 = meds
kmedoidIter dists meds0 iter = kmedoidIter dists meds1 (iter-1) where
  clusters = assignToClosestMedoid dists meds0
  meds1 = map (\(_, pts)-> reassignMedoid dists pts) $ Map.toList clusters


allKeys :: DistMap -> [Int]
allKeys = map fst . Map.keys

getDist :: DistMap -> Int -> Int -> Double
getDist dists from to
  | from == to = 0
  | otherwise =
    case Map.lookup (from, to) dists of
      Just d -> d
      Nothing -> case Map.lookup (to,from) dists of
        Just d -> d
        Nothing -> error $ "cannot find distance from "++show from ++" to "++show to

assignToClosestMedoid :: DistMap -> [Int] -> Map.Map Int [Int]
assignToClosestMedoid dists medoids = clusmap where
  assignmap = Map.fromList $ map (\k -> (k, findNearestMedoid dists medoids k)) $ allKeys dists
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

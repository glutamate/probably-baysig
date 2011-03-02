--stolen from kmeans!

module Math.Probably.KMeans (kmeansOn, kmeans')
    where
import Data.List (transpose, sort, groupBy, minimumBy)
import Data.Function (on)
import Data.Ord (comparing)
--import Numeric.LinearAlgebra

type Vector a = [a]


dist :: (Floating b) => Vector b -> Vector b -> b
dist a b = sqrt . sum $ zipWith (\x y-> (x-y) ^ 2) a b


centroid :: (Fractional b) => (a -> Vector b) -> [a] -> Vector b
centroid f points = map (flip (/) l . sum) $ transpose $ map f points
    where l = fromIntegral $ length points

closest :: (Ord b, Floating b) => [Vector b] -> Vector b -> Vector b
closest points point = minimumBy (comparing $ dist point) points

recluster'
  :: (Ord b, Ord a, Floating b) =>
     (a -> Vector b) -> [Vector b] -> [a] -> [[a]]
recluster' f centroids points = map (map snd) $ groupBy ((==) `on` fst) reclustered
    where reclustered = sort [(closest centroids $ f a, a) | a <- points]

recluster :: (Floating a, Ord a, Ord b) => (b->Vector a) -> [[b]] -> [[b]]
recluster f clusters = recluster' f centroids $ concat clusters
    where centroids = map (centroid f) clusters

part :: (Eq a) => Int -> [a] -> [[a]]
part x ys
     | zs' == [] = [zs]
     | otherwise = zs : part x zs'
    where (zs, zs') = splitAt x ys
-- | Recluster points
kmeans' :: (Floating a, Ord a, Eq b, Ord b) => (b->Vector a) -> [[b]] -> [[b]]
kmeans' f clusters
    | clusters == clusters' = clusters
    | otherwise             = kmeans' f clusters'
    where clusters' = recluster f clusters
-- | Cluster points into k clusters.
-- |
-- | The initial clusters are chosen arbitrarily
kmeansOn :: (Floating a, Ord a, Eq b, Ord b) => (b->Vector a) -> Int -> [b] -> [[b]]
kmeansOn f k points = kmeans' f $ part l points
    where l = (length points + k - 1) `div` k
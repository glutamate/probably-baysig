--stolen from kemans!

module Math.Probably.KMeans (kmeansOn, kmeans')
    where
import Data.List (transpose, sort, groupBy, minimumBy)
import Data.Function (on)
import Data.Ord (comparing)
type Vector a = [a]
dist a b = sqrt . sum $ zipWith (\x y-> (x-y) ^ 2) a b
centroid f points = map (flip (/) l . sum) $ transpose $ map f points
    where l = fromIntegral $ length points
closest points point = minimumBy (comparing $ dist point) points
recluster' f centroids points = map (map snd) $ groupBy ((==) `on` fst) reclustered
    where reclustered = sort [(closest centroids $ f a, a) | a <- points]
recluster f clusters = recluster' f centroids $ concat clusters
    where centroids = map (centroid f) clusters
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
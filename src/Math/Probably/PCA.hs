module Math.Probably.PCA where

import Numeric.LinearAlgebra
import Control.Spoon

empiricalCovariance :: [Vector Double] -> Matrix Double
empiricalCovariance xs
   = let k = length xs
         --barxk = L.scale (recip $ realToFrac $ k+1) $ sum xs
         --m1 = sum $ map (\xv-> L.outer xv xv) xs
         --m2 = L.scale (realToFrac $ k+1) $ L.outer barxk barxk
         xmn = empiricalMean xs
         f xi = outerSame $ xi - xmn
--     in L.scale (recip $ realToFrac k) $ m1 - m2
     in scale (recip $ realToFrac k - 1 ) $ sum $ map f xs

outerSame v = outer v v

empiricalMean :: [Vector Double] -> Vector Double
empiricalMean vecs = scale (recip $ realToFrac $ length vecs) $ sum vecs

-- copied from https://github.com/albertoruiz/hmatrix/blob/master/examples/pca2.hs

type Vec = Vector Double
type Mat = Matrix Double

-- Vector with the mean value of the columns of a matrix
mean a = constant (recip . fromIntegral . rows $ a) (rows a) <> a

-- covariance matrix of a list of observations stored as rows
cov x = (trans xc <> xc) / fromIntegral (rows x - 1)
    where xc = x - asRow (mean x)


type Stat = (Vec, [Double], Mat)
-- 1st and 2nd order statistics of a dataset (mean, eigenvalues and eigenvectors of cov)
stat :: Mat -> Stat
stat x = (m, toList s, trans v) where
    m = mean x
    (s,v) = eigSH' (cov x)

mbStat :: Mat -> Maybe Stat
mbStat = spoon . stat

pca :: Double -> Stat -> (Vec -> Vec , Vec -> Vec)
pca prec (m,s,v) = (encode,decode)
  where
    encode x = vp <> (x - m)
    decode x = x <> vp + m
    vp = takeRows n v
    n = 1 + (length $ fst $ span (< (prec'*sum s)) $ cumSum s)
    cumSum = tail . scanl (+) 0.0
    prec' = if prec <=0.0 || prec >= 1.0
                then error "the precision in pca must be 0<prec<1"
                else prec

pcaN :: Int -> Stat -> (Vec -> Vec , Vec -> Vec)
pcaN n (m,s,v) = (encode,decode)
  where
    encode x = vp <> (x - m)
    decode x = x <> vp + m
    vp = takeRows n v

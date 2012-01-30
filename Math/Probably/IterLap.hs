{-# LANGUAGE ScopedTypeVariables #-}


module Math.Probably.IterLap where

import Math.Probably.Sampler
import Math.Probably.FoldingStats
import Math.Probably.MCMC (empiricalMean, empiricalCovariance)
import Debug.Trace

import Numeric.LinearAlgebra hiding (find)

weightedMeanCov :: [(Vector Double, Double)] -> (Vector Double, Matrix Double)
weightedMeanCov pts = (mu, cov) where
  mu = sum $ flip map pts $ \(x,w) -> scale w x
  npts = dim $ fst $ head pts
  sumSqrWs = sum $ flip map pts $ \(x,w) -> w*w
  factor = 1/(1-sumSqrWs) 
  xmeans :: [Double]
  xmeans = flip map [0..npts-1] $ \i -> mean $ flip map pts $ \(x,w) -> x@>i
  cov = scale factor $ buildMatrix npts npts $ \(j,k) -> 
         sum $ flip map pts $ \(xi,wi) -> wi*(xi@>j - xmeans!!j)*
                                             (xi@>k - xmeans!!k)

mean :: [Double] -> Double
mean xs = sum xs / realToFrac (length xs) 

improve :: Int -> (Vector Double -> Double) 
        -> (Vector Double, Matrix Double)
        -> Sampler (Vector Double, Matrix Double)
improve n f (mu, cov) = do
  xs <- sequence $ replicate n $ multiNormal mu cov
  let ps = map f xs
      pmin = foldl1 min ps
      psum = sum $ map (\p-> p - pmin) ps
      ws =  map (\p-> (p-pmin)/psum) ps
      (mu', cov') = weightedMeanCov $zip xs ws
  return $ (mu', scale 2 cov')

iterateM :: Int -> (a -> Sampler a) -> a-> Sampler a
iterateM 0 _ x = return x
iterateM n f x = do
   newx <- f x
   iterateM (n-1) f newx

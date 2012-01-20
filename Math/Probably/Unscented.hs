{-# LANGUAGE ScopedTypeVariables #-}


module Math.Probably.Unscented where

import Math.Probably.Sampler
import Math.Probably.FoldingStats
import Math.Probably.MCMC


import Numeric.LinearAlgebra hiding (find)

-- smells because it is inefficient. For testing.
smellyTransform :: Sampler a -> Int -> (a -> Vector Double) -> Sampler (Vector Double, Matrix Double)
smellyTransform dist n f = do
   ys  <- sequence $ replicate n $ fmap f dist
   let n = dim $ head ys
   return (empiricalMean ys, empiricalCovariance ys)
   

unscentedTransform :: (Vector Double, Matrix Double) -> 
                      (Vector Double -> Vector Double) -> 
                      (Vector Double, Matrix Double)
unscentedTransform (xmean, xcov) f = (ymean, ycov) where
   n = dim xmean
   nr = (realToFrac n) :: Double
   k = 3-nr
   matSqrt = toRows $ chol ( scale (nr+k) xcov)
   xs = concat [[xmean], 
                [xmean + (matSqrt!!i)| i <- [0..(n-1)]],
                [xmean - (matSqrt!!i)| i <- [0..(n-1)]]]
   ws = (k / (nr+k)) : replicate (2*n) (1/(2*(nr+k)))
   ys = map f xs
   ymean = sum $ zipWith (scale) ws ys
   ycov = sum [scale wi $ (yi - ymean) `outer` (yi - ymean) | (wi,yi) <- zip ws ys]
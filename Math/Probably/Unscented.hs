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
unscentedTransform (mean, cov) f = (mean1, cov1) where
   n = dim mean
   k = n-3
   matSqrt = toRows $ sqrt ( )
   xs = concat [mean, [ mean + matSqrt!!i| i <- [0..(n-1)]], []]
   ws = (k / (n+k)) : replicate (2*n) (1/(2*(n+k)))
   mean1 = undefined
   cov1 = undefined
{-# LANGUAGE ScopedTypeVariables #-}


module Math.Probably.Unscented where

import Math.Probably.Sampler
import Math.Probably.FoldingStats
import Math.Probably.MCMC (empiricalMean, empiricalCovariance)
import Debug.Trace

import Numeric.LinearAlgebra hiding (find)

-- smells because it is inefficient. For testing.
smellyTransform :: Sampler a -> Int -> (a -> Vector Double) -> Sampler (Vector Double, Matrix Double)
smellyTransform dist n f = do
   ys  <- sequence $ replicate n $ fmap f dist
   let n = dim $ head ys
   return (empiricalMean ys, empiricalCovariance ys)
   
alphaUT = 1e-3
kappaUT = 0
betaUT = 2

unscentedTransform :: (Vector Double, Matrix Double) -- ^ random variable
                   -> (Vector Double -> Vector Double) -- ^ nonlinear transformation
                   -> (Vector Double, Matrix Double, [Vector Double])
unscentedTransform (xmean, xcov) f = (ymean, ycov, xs) where
   l = dim xmean
   lr = (realToFrac l) :: Double
   lambda = alphaUT * alphaUT * (lr + kappaUT) - lr
   matSqrt = toRows $ trans $ chol $ scale (lr+lambda) xcov
   xs = concat [[xmean], 
                [xmean + (matSqrt!!i)| i <- [0..(l-1)]],
                [xmean - (matSqrt!!i)| i <- [0..(l-1)]]]
   wm = (lambda / (lr+lambda)) : replicate (2*l) (1/(2*(lr+lambda)))
   wc = ((lambda / (lr+lambda)) + 1 - alphaUT*alphaUT +betaUT) : replicate (2*l) (1/(2*(lr+lambda)))
   ys = map f xs
   ymean = sum $ zipWith (scale) wm ys
   ycov = sum [scale wi $ (yi - ymean) `outer` (yi - ymean) | (wi,yi) <- zip wc ys]


unscentedKalmanFilterAdditive :: (Vector Double -> Vector Double) -- ^ processes nonlinearity
                      -> (Vector Double -> Vector Double) -- ^ observation nonlinearity
                      -> Matrix Double -- ^ process noise
                      -> Matrix Double -- ^ observation noise
                      -> (Vector Double, Matrix Double) -- ^ initial state estimate
                      -> [Vector Double] -- ^ observations
                      -> [(Vector Double, Matrix Double)] 
unscentedKalmanFilterAdditive procF obsF procCov obsCov (xmn0, xcov0) [] = []
unscentedKalmanFilterAdditive procF obsF procCov obsCov (xmn0, xcov0) (yobs:yobss) =
           (xmn1, xcov1) : unscentedKalmanFilterAdditive procF obsF procCov obsCov (xmn1, xcov1) (yobss) where
  nx = dim xmn0
  nrx = realToFrac $ nx
  kx = 3-nrx

  xmn1zz = ymn1pred
  xcov1zz = ycov1pred

  (xmn1pred, xcov1pred', _) = unscentedTransform (xmn0, xcov0) procF 
  xcov1pred = xcov1pred' + procCov

  (ymn1pred, ycov1pred', chis) = unscentedTransform (xmn1pred, xcov1pred) obsF 
  ycov1pred = ycov1pred' + obsCov

  ny = dim ymn1pred
  nry = realToFrac $ ny
  ky = 3-nry

  ws = (ky / (nry+ky)) : replicate (2*ny) (1/(2*(nry+ky)))
-- $ trace (show $ map dim [chii,xmn1pred, obsF chii, ymn1pred])
  crossCov = traceit "cross " $ sum [scale wi 
                                      $ (chii - xmn1pred) `outer` 
                                        (obsF chii - ymn1pred) 
                      | (wi,chii) <- zip ws chis]

  gain = crossCov `multiply` inv ycov1pred

  xmn1 = xmn1pred + gain `mXv` (yobs - ymn1pred)
  xcov1 = traceit "newxcov " $ xcov1pred - gain `multiply` ycov1pred `multiply` trans gain 


traceit s x = trace (s++" : "++show x) x


simDynamical :: Int 
             -> (Vector Double -> Vector Double) -- ^ processes nonlinearity
             -> (Vector Double -> Vector Double) -- ^ observation nonlinearity
             -> Matrix Double -- ^ process noise
             -> Matrix Double -- ^ observation noise
             -> Vector Double -- ^ initial state
             -> Sampler [(Vector Double, Vector Double)]

simDynamical 0 procF obsF procCov obsCov x0 = return []
simDynamical n procF obsF procCov obsCov x0 = do
        xnoise <- multiNormal (constant 0$ dim x0) procCov
        let x1 = procF $ join [ x0, xnoise]
        ynoise <- multiNormal (constant 0$ rows obsCov) obsCov
        let y1 = obsF $ join [ x1, ynoise]
        rest <- simDynamical (n-1) procF obsF procCov obsCov x1 
        return $ (x1, y1) : rest


unscentedKalmanFilter :: (Vector Double -> Vector Double) -- ^ processes nonlinearity
                      -> (Vector Double -> Vector Double) -- ^ observation nonlinearity
                      -> Matrix Double -- ^ process noise
                      -> Matrix Double -- ^ observation noise
                      -> (Vector Double, Matrix Double) -- ^ initial state estimate
                      -> [Vector Double] -- ^ observations
                      -> [(Vector Double, Matrix Double)] 
unscentedKalmanFilter procF obsF procCov obsCov (xmn0, xcov0) [] = []
unscentedKalmanFilter procF obsF procCov obsCov (xmn0, xcov0) (yobs:yobss) =
           (xmn1, xcov1) : unscentedKalmanFilter procF obsF procCov obsCov (xmn1, xcov1) (yobss) where
  nx = dim xmn0
  nrx = realToFrac $ nx
  kx = 3-nrx

  xmn1zz = ymn1pred
  xcov1zz = ycov1pred

  xmn0aug = traceit "xm0aug" $ join [xmn0, constant 0 nx]
  xcov0aug = fromBlocks [[xcov0, 0], 
                         [0,     procCov]]
  (xmn1pred, xcov1pred, _) = unscentedTransform (xmn0aug, xcov0aug) procF 

  xmn1aug = traceit "xm1aug"$ join [xmn1pred, constant 0 nx]
  xcov1aug = fromBlocks [[xcov1pred, 0], 
                         [0,         obsCov]]


  (ymn1pred, ycov1pred, chis) = unscentedTransform (xmn1aug, xcov1aug) obsF 

  ny = dim ymn1pred
  nry = realToFrac $ ny
  ky = 3-nry

  ws = (ky / (nry+ky)) : replicate (2*ny) (1/(2*(nry+ky)))
-- $ trace (show $ map dim [chii,xmn1pred, obsF chii, ymn1pred])
  crossCov = traceit "cross " $ sum [scale wi 
                                      $ (subVector 0 nx chii - xmn1pred) `outer` 
                                        (obsF chii - ymn1pred) 
                      | (wi,chii) <- zip ws chis]

  gain = crossCov `multiply` inv ycov1pred

  xmn1 = xmn1pred + gain `mXv` (yobs - ymn1pred)
  xcov1 = traceit "newxcov " $ xcov1pred - gain `multiply` ycov1pred `multiply` trans gain 

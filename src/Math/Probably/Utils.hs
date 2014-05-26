
-- | Internal utilities.

module Math.Probably.Utils where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Vector.Storable as V
import Math.Probably.Types
import Numeric.LinearAlgebra
import Statistics.Distribution
import Statistics.Distribution.Normal

lookupDefault  :: Ord k => a -> k -> Map k a -> a
lookupDefault d k m = fromMaybe d (Map.lookup k m)

-- | A spherical Gaussian distribution.
sphereGauss :: ContinuousParams -> ContinuousParams -> Double -> Double
sphereGauss xs m sd = product $ zipWith density normalDists xsAsList where
  xsAsList    = toList xs
  meanAsList  = toList m
  normalDists = map (`normalDistr` sd) meanAsList

-- | Scalar-vector multiplication.
(.*) :: Double -> ContinuousParams -> ContinuousParams
z .* xs = mapVector (* z) xs

-- | Scalar-vector subtraction.
(.-) :: ContinuousParams -> ContinuousParams -> ContinuousParams
xs .- ys = zipVectorWith (-) xs ys

-- | Scalar-vector addition.
(.+) :: ContinuousParams -> ContinuousParams -> ContinuousParams
xs .+ ys = zipVectorWith (+) xs ys

-- | The leapfrog integrator.
leapfrog :: Gradient -> Particle -> Double -> Particle
leapfrog glTarget (q, r) e = (qf, rf) where 
  rm = adjustMomentum glTarget e (q, r)
  qf = adjustPosition e (rm, q)
  rf = adjustMomentum glTarget e (qf, rm)

-- | Adjust momentum according to a half-leapfrog step.
adjustMomentum :: Gradient -> Double -> Particle -> ContinuousParams
adjustMomentum glTarget e (t, r) = r .+ ((e / 2) .* glTarget t)

-- | Adjust position according to a half-leapfrog step.
adjustPosition :: Double -> Particle -> ContinuousParams
adjustPosition e (r, t) = t .+ (e .* r)

-- | A target augmented by momentum auxilliary variables.
auxilliaryTarget :: (ContinuousParams -> Double) -> Particle -> Double
auxilliaryTarget lTarget (t, r) =
  lTarget t - 0.5 * innerProduct r r

innerProduct :: ContinuousParams -> ContinuousParams -> Double
innerProduct xs ys = V.sum $ V.zipWith (*) xs ys

indicate :: Integral a => Bool -> a
indicate True  = 1
indicate False = 0

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral


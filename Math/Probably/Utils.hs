module Math.Probably.Utils where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Numeric.LinearAlgebra
import Statistics.Distribution
import Statistics.Distribution.Normal

type Parameters = Vector Double
type Gradient   = Parameters -> Parameters
type Particle   = (Parameters, Parameters)

lookupDefault  :: Ord k => a -> k -> Map k a -> a
lookupDefault d k m = fromMaybe d (Map.lookup k m)

sphereGauss :: Vector Double -> Vector Double -> Double -> Double
sphereGauss xs mu sd = product $ zipWith density normalDists xsAsList where
  xsAsList    = toList xs
  muAsList    = toList mu
  normalDists = map (`normalDistr` sd) muAsList

(.*) :: Double -> Vector Double -> Vector Double
z .* xs = mapVector (* z) xs

(.-) :: Vector Double -> Vector Double -> Vector Double
xs .- ys = zipVectorWith (-) xs ys

(.+) :: Vector Double -> Vector Double -> Vector Double
xs .+ ys = zipVectorWith (+) xs ys

leapfrog :: Gradient -> Particle -> Double -> Particle
leapfrog glTarget (t, r) e = (tf, rf) where 
  rm = adjustMomentum glTarget e t r
  tf = adjustPosition e rm t
  rf = adjustMomentum glTarget e tf rm

adjustMomentum :: (t -> Parameters) -> Double -> t -> Parameters -> Parameters
adjustMomentum glTarget e t r = r .+ ((e / 2) .* glTarget t)

adjustPosition :: Double -> Parameters -> Parameters -> Parameters
adjustPosition e r t = t .+ (e .* r)

auxilliaryTarget :: (t -> Double) -> t -> Parameters -> Double
auxilliaryTarget lTarget t r = exp (lTarget t - 0.5 * innerProduct r r)

innerProduct :: Parameters -> Parameters -> Double
innerProduct xs ys = V.sum $ V.zipWith (*) xs ys

indicate :: Integral a => Bool -> a
indicate True  = 1
indicate False = 0

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral


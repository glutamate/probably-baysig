module Math.Probably.Utils where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Numeric.LinearAlgebra
import Statistics.Distribution
import Statistics.Distribution.Normal

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


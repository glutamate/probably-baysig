module Strategy.MetropolisHastings (metropolisHastings) where

import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as Map
import Numeric.LinearAlgebra
import Math.Probably.Sampler
import Math.Probably.Types
import Math.Probably.Utils
import Statistics.Distribution
import Statistics.Distribution.Normal

sphericalGaussian :: Vector Double -> Vector Double -> Double -> Double
sphericalGaussian xs mu sd = product $ zipWith density normalDists xsAsList
  where
    xsAsList    = toList xs
    muAsList    = toList mu
    normalDists = map (`normalDistr` sd) muAsList

perturb
  :: Vector Double
  -> Double
  -> Prob (Vector Double)
perturb q sd = fmap fromList $ mapM (`normal` sd) (toList q)

acceptRatio
  :: Target Double -> Vector Double -> Vector Double -> Double -> Double
acceptRatio target current proposed sd = exp . min 0 $
    logObjective target proposed + log (sphericalGaussian current proposed sd)
  - logObjective target current  - log (sphericalGaussian proposed current sd)

nextState
  :: Target Double
  -> Vector Double
  -> Vector Double
  -> Double
  -> Double
  -> Vector Double
nextState target current proposed sd z
    | z < acceptProb = proposed
    | otherwise      = current
  where
    ratio = acceptRatio target current proposed sd 
    acceptProb | isNaN ratio = 0
               | otherwise   = ratio

getStandardDeviation :: Maybe Double -> Tunables -> Double
getStandardDeviation (Just sd) _   = sd
getStandardDeviation Nothing store = sd where
  (TDouble sd) = lookupDefault (TDouble 1.0) MH store

updateStandardDeviation :: Double -> Tunables -> Tunables
updateStandardDeviation sd = Map.insert MH (TDouble sd) 

metropolisHastings :: Maybe Double -> Transition Double
metropolisHastings e = do
  Chain current target _ store <- get
  let sd = getStandardDeviation e store
  proposed <- lift $ perturb current sd
  zc       <- lift unit
  let next     = nextState target current proposed sd zc
      newStore = updateStandardDeviation sd store
  put $ Chain next target (logObjective target next) newStore
  return next


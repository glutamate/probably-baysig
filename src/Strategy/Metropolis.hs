
-- | The Metropolis transition.

module Strategy.Metropolis (metropolis) where

import Control.Monad.State.Strict
import Data.Maybe
import Numeric.LinearAlgebra
import Math.Probably.Sampler
import Math.Probably.Types
import Math.Probably.Utils

-- | The Metropolis transition for a symmetric proposal.  Uses a simple
--   spherical Gaussian with standard deviation specified by the first
--   argument.
metropolis :: Maybe Double -> Transition Double
metropolis e = do
  Chain current@(ds, cs) target _ t <- get
  let sd = fromMaybe t e
  pcs <- lift $ perturb cs sd
  zc  <- lift unit
  let next = nextState target current (ds, pcs) sd zc
  put $ Chain next target (logObjective target next) sd
  return next

perturb :: ContinuousParams -> Double -> Prob (ContinuousParams)
perturb q sd = fmap fromList $ mapM (`normal` sd) (toList q)

acceptRatio :: Target -> Parameters -> Parameters -> Double -> Double
acceptRatio target c@(_, current) p@(_, proposed) sd = exp . min 0 $
    logObjective target p + log (sphereGauss current proposed sd)
  - logObjective target c - log (sphereGauss proposed current sd)

nextState
  :: Target
  -> Parameters
  -> Parameters
  -> Double
  -> Double
  -> Parameters
nextState target current proposed sd z
    | z < acceptProb = proposed
    | otherwise      = current
  where
    ratio = acceptRatio target current proposed sd 
    acceptProb | isNaN ratio = 0
               | otherwise   = ratio
 


-- | Metropolis-adjusted Langevin diffusion.  See, ex: Girolami and Calderhead
--   (2011):
--
-- http://onlinelibrary.wiley.com/doi/10.1111/j.1467-9868.2010.00765.x/abstract

module Strategy.MALA (mala, adaMala) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Maybe
import Math.Probably.Sampler
import Math.Probably.Types
import Math.Probably.Utils
import Numeric.LinearAlgebra

-- | The MALA transition operator.  Takes a step size tunable parameter.
mala :: Maybe Double -> Transition Double
mala e = do
  Chain current@(ds, cs) target _ t <- get
  let stepSize = fromMaybe t e
  pcs <- lift $ perturb target cs stepSize
  zc  <- lift unit

  let cMean = localMean target cs stepSize
      pMean = localMean target pcs stepSize
      next  = nextState target current cMean (ds, pcs) pMean stepSize zc

  put $ Chain next target (logObjective target next) stepSize
  return next

adaMala :: Transition (Maybe (Double,Int))
adaMala = do
  Chain current@(ds, cs) target _ madaPars <- get
  let (stepSize, trCount) = case madaPars of
          Nothing -> (0.1, 1)
          Just st -> st
  pcs <- lift $ perturb target cs stepSize
  zc  <- lift unit

  let cMean = localMean target cs stepSize
      pMean = localMean target pcs stepSize
      ratio = acceptProb target current cMean (ds, pcs) pMean stepSize
      pAccent | isNaN ratio = 0
              | otherwise   = ratio

  if zc<pAccent
     then do let stepNext = (min 1.4 $ 1+kmala/(realToFrac trCount))*stepSize
             put $ Chain (ds, pcs) target (logObjective target (ds, pcs)) $ Just (stepNext, trCount+1)
             return (ds, pcs)
             
     else do let stepNext = (max 0.7143 $ 1-kmala/(realToFrac trCount))**1.3*stepSize
             put $ Chain (ds, cs) target (logObjective target (ds, cs)) $ Just (stepNext, trCount+1)
             return (ds, cs)

kmala :: Double
kmala = 5

localMean :: Target -> Vector Double -> Double -> Vector Double
localMean target position e = position .+ scaledGradient position where
  grad = handleGradient (gradient target)
  scaledGradient p = (0.5 * e * e) .* grad p

perturb
  :: Target
  -> ContinuousParams
  -> Double
  -> Prob ContinuousParams
perturb target position e = do
  zs <- fromList <$> replicateM (dim position) unormal
  return $ localMean target position e .+ (e .* zs)

nextState
  :: Target
  -> Parameters
  -> ContinuousParams
  -> Parameters
  -> ContinuousParams
  -> Double
  -> Double
  -> Parameters
nextState target current cMean proposal pMean e z
    | z < pAccent = proposal
    | otherwise   = current
  where
    ratio = acceptProb target current cMean proposal pMean e
    pAccent | isNaN ratio = 0
            | otherwise   = ratio

acceptProb
  :: Target
  -> Parameters
  -> ContinuousParams
  -> Parameters
  -> ContinuousParams
  -> Double
  -> Double
acceptProb target current@(_, cs) cMean proposal@(_, pcs) pMean e =
  exp . min 0 $
      logObjective target proposal + log (sphereGauss cs pMean e)
    - logObjective target current  - log (sphereGauss pcs cMean e)


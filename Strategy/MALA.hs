
module Strategy.MALA (mala) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Math.Probably.Sampler
import Math.Probably.Types
import Math.Probably.Utils
import Numeric.LinearAlgebra
import Statistics.Distribution
import Statistics.Distribution.Normal

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

mala :: Maybe Double -> Transition Double
mala e = do
  Chain current@(ds, cs) target _ t <- get
  let step = fromMaybe t e
  pcs <- lift $ perturb target cs step
  zc  <- lift unit

  let cMean = localMean target cs step
      pMean = localMean target pcs step
      next  = nextState target current cMean (ds, pcs) pMean step zc

  put $ Chain next target (logObjective target next) step
  return next

nextState
  :: Target
  -> Parameters
  -> ContinuousParams
  -> Parameters
  -> ContinuousParams
  -> Double
  -> Double
  -> Parameters
nextState target current@(_, cs) cMean proposal@(_, pcs) pMean e z
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


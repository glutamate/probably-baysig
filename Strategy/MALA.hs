
module Strategy.MALA (mala) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as Map
import Math.Probably.Sampler
import Math.Probably.Types
import Math.Probably.Utils
import Numeric.LinearAlgebra
import Statistics.Distribution
import Statistics.Distribution.Normal

localMean :: Target Double -> Vector Double -> Double -> Vector Double
localMean target position e = position .+ scaledGradient position where
  grad = handleGradient (gradient target)
  scaledGradient p = (0.5 * e * e) .* grad p

perturb
  :: Target Double
  -> Vector Double
  -> Double
  -> Prob (Vector Double)
perturb target position e = do
  zs <- fromList <$> replicateM (dim position) unormal
  return $ localMean target position e .+ (e .* zs)

mala e = do
  Chain current target _ store <- get
  let step = getStepSize e store
  proposal <- lift $ perturb target current step
  zc       <- lift unit

  let cMean    = localMean target current step
      pMean    = localMean target proposal step
      next     = nextState target (current, cMean) (proposal, pMean) step zc
      newStore = updateStepSize step store

  put $ Chain next target (logObjective target next) newStore
  return next

getStepSize :: Maybe Double -> Tunables -> Double
getStepSize (Just step) _ = step
getStepSize Nothing store = step where
  (TDouble step) = lookupDefault (TDouble 1.0) MALA store

updateStepSize :: Double -> Tunables -> Tunables
updateStepSize step = Map.insert MALA (TDouble step) 

nextState
  :: Target Double
  -> (Vector Double, Vector Double)
  -> (Vector Double, Vector Double)
  -> Double
  -> Double
  -> Vector Double
nextState target (current, cMean) (proposal, pMean) e z
    | z < acceptProb = proposal
    | otherwise      = current
  where
    ratio = acceptRatio target (current, cMean) (proposal, pMean) e 
    acceptProb | isNaN ratio = 0
               | otherwise   = ratio

acceptRatio
  :: Target Double
  -> (Vector Double, Vector Double)
  -> (Vector Double, Vector Double)
  -> Double
  -> Double
acceptRatio target (current, cMean) (proposal, pMean) e = exp . min 0 $
    logObjective target proposal + log (sphereGauss current pMean e)
  - logObjective target current  - log (sphereGauss proposal cMean e)


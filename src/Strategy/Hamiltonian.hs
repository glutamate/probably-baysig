
-- | Hamiltonian Monte Carlo.  See, ex: Neal (2012)
--   http://arxiv.org/pdf/1206.1901.pdf.

module Strategy.Hamiltonian (hamiltonian) where

import Control.Monad.State.Strict
import Data.Maybe
import qualified Data.Vector.Storable as V
import Math.Probably.Sampler
import Math.Probably.Types
import Math.Probably.Utils
import Numeric.LinearAlgebra

-- | A Hamiltonian transition with step size and leapfrog increment tunables.
hamiltonian :: Maybe Double -> Maybe Int -> Transition (Double, Int)
hamiltonian e l = do
  Chain (ds, q0) target _ (te, tl) <- get
  let stepSize = fromMaybe te e
      nDisc    = fromMaybe tl l
      lTarget  = curry (logObjective target) ds
      glTarget = handleGradient $ gradient target

  r0 <- V.replicateM (dim q0) (lift unormal)
  zc <- lift unit
  let (q, r) = leapfrogIntegrator glTarget (q0, r0) stepSize nDisc
      next   = (ds, nextState lTarget (q0, q) (r0, r) zc)

  put $ Chain next target (logObjective target next) (stepSize, nDisc)
  return next

-- | The leapfrog or Stormer-Verlet integrator.
leapfrogIntegrator :: Gradient -> Particle -> Double -> Int -> Particle
leapfrogIntegrator glTarget (q0, r0) e = go q0 r0 where
  go q r 0 = (q, r)
  go q r n = let (q1, r1) = leapfrog glTarget (q, r) e
             in  go q1 r1 (pred n)

-- | The acceptance probability of a move.
acceptProb :: (Vector Double -> Double) -> Particle -> Particle -> Double
acceptProb lTarget (q0, q1) (r0, r1) = exp . min 0 $
  auxilliaryTarget lTarget (q1, r1) - auxilliaryTarget lTarget (q0, r0)

-- | The next state, given a proposal and random value from (0, 1).
nextState
  :: (Vector Double -> Double)
  -> Particle
  -> Particle
  -> Double
  -> ContinuousParams
nextState lTarget position momentum z
    | z < pAccept = snd position
    | otherwise   = fst position
  where
    pAccept = acceptProb lTarget position momentum


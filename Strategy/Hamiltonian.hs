module Strategy.Hamiltonian (hamiltonian) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans
import qualified Data.Map as Map
import Data.Maybe
import Math.Probably.Sampler
import Math.Probably.Types
import Math.Probably.Utils
import Numeric.LinearAlgebra

leapfrogIntegrator
  :: Target
  -> Particle
  -> Double
  -> Int
  -> Particle
leapfrogIntegrator target (q0, r0) e = go q0 r0 where
  go q r 0 = (q, r)
  go q r n = let (q1, r1) = leapfrog target (q, r) e
             in  go q1 r1 (pred n)

acceptProb
  :: (Vector Double -> Double)
  -> Particle
  -> Particle
  -> Double
acceptProb lTarget (q0, q1) (r0, r1) = exp . min 0 $
    auxilliaryTarget lTarget (q1, r1) - auxilliaryTarget lTarget (q0, r0)

nextState
  :: (Vector Double -> Double)
  -> Particle
  -> Particle
  -> Double
  -> ContinuousParams
nextState lTarget (q0, q1) (r0, r1) z
    | z < pAccept = q1
    | otherwise   = q0
  where
    pAccept = acceptProb lTarget (q0, q1) (r0, r1)

hamiltonian :: Maybe Double -> Maybe Int -> Transition (Double, Int)
hamiltonian e l = do
  Chain current@(ds, q0) target _ (te, tl) <- get
  let stepSize = fromMaybe te e
      nDisc    = fromMaybe tl l
      lTarget  = curry (logObjective target) ds

  r0 <- fromList <$> replicateM (dim q0) (lift unormal)
  zc <- lift unit

  let (q, r) = leapfrogIntegrator target (q0, r0) stepSize nDisc
      next   = (ds, nextState lTarget (q0, q) (r0, r) zc)

  put $ Chain next target (logObjective target next) (stepSize, nDisc)
  return next


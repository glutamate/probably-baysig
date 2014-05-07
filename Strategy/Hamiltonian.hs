module Strategy.Hamiltonian (hamiltonian) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans
import qualified Data.Map as Map
import Math.Probably.Sampler
import Math.Probably.Types
import Math.Probably.Utils hiding (leapfrog, adjustMomentum, adjustPosition, auxilliaryTarget)
import Numeric.LinearAlgebra

leapfrog
  :: Target Double
  -> Vector Double
  -> Vector Double
  -> Double
  -> (Vector Double, Vector Double)
leapfrog target q r e = (qf, rf) where 
  rm       = adjustMomentum glTarget e q r
  qf       = adjustPosition e rm q
  rf       = adjustMomentum glTarget e qf rm
  glTarget = handleGradient $ gradient target

leapfrogIntegrator
  :: Target Double
  -> Vector Double
  -> Vector Double
  -> Double
  -> Int
  -> (Vector Double, Vector Double)
leapfrogIntegrator target q0 r0 e = go q0 r0 where
  go q r 0 = (q, r)
  go q r n = let (q1, r1) = leapfrog target q r e
             in  go q1 r1 (pred n)

adjustMomentum
  :: (Vector Double -> Vector Double)
  -> Double
  -> Vector Double
  -> Vector Double
  -> Vector Double
adjustMomentum glTarget e q r = r .+ ((0.5 * e) .* glTarget q)

adjustPosition :: Double -> Vector Double -> Vector Double -> Vector Double
adjustPosition e r q = q .+ (e .* r)

acceptanceRatio
  :: (t -> Double)
  -> t
  -> t
  -> Vector Double
  -> Vector Double
  -> Double
acceptanceRatio lTarget q0 q1 r0 r1 =
  auxilliaryTarget lTarget q1 r1 - auxilliaryTarget lTarget q0 r0

auxilliaryTarget :: (t -> Double) -> t -> Vector Double -> Double
auxilliaryTarget lTarget q r = lTarget q - 0.5 * innerProduct r r

nextState
  :: Double
  -> Target Double
  -> Vector Double
  -> Vector Double
  -> Vector Double
  -> Vector Double
  -> Vector Double
nextState z target q0 q1 r0 r1
    | z < min 1 ratio = q1
    | otherwise       = q0
  where
    ratio = exp $ acceptanceRatio (logObjective target) q0 q1 r0 r1

-- should match other patterns
getParameters :: Maybe Double -> Maybe Int -> Tunables -> (Double, Int)
getParameters (Just e) (Just l) _ = (e, l)
getParameters _  _          store = (e, l) where
  TPair (TDouble e, TInt l) =
    lookupDefault (TPair (TDouble 0.05, TInt 20)) HMC store

updateParameters :: Double -> Int -> Tunables -> Tunables
updateParameters e l = Map.insert HMC (TPair (TDouble e, TInt l)) 

hamiltonian :: Maybe Double -> Maybe Int -> Transition Double
hamiltonian e l = do
  Chain current target _ store <- get
  let (stepSize, nDisc) = getParameters e l store
      q0                = current
  r0 <- fromList <$> replicateM (dim q0) (lift unormal)

  zc <- lift unit
  let (q, r)   = leapfrogIntegrator target q0 r0 stepSize nDisc
      next     = nextState zc target q0 q r0 r
      newStore = updateParameters stepSize nDisc store
  put $ Chain next target (logObjective target next) newStore
  return next


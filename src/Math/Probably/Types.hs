{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Basic types used throughout the library.
--
--   Includes a probability type (based on sampling functions, with a tweak for
--   collected samples) and associated instances, as well as types used for
--   MCMC.

module Math.Probably.Types where

import Control.Applicative
import Control.Monad.State.Strict
import Numeric.LinearAlgebra
import System.Random.Mersenne.Pure64

type Seed = PureMT

data Prob a =
    Sampler { unSampler :: Seed -> (a, Seed) }
  | Samples [a]

instance Functor Prob where
  fmap f (Sampler sf) = Sampler $ \rs -> let (x,rs') = sf rs in (f x, rs')
  fmap f (Samples xs) = Samples $ map f xs
 
instance Applicative Prob where
  pure x = Sampler (\rs-> (x, rs))
  (Sampler sff) <*> (Sampler sfx) = Sampler $ \rs -> 
    let (f ,rs') = sff rs 
        (x, rs'') = sfx rs' 
    in (f x, rs'')
  _ <*> _ = error "Prob (<*>): unsupported pattern"

instance Monad Prob where
  return = pure
  (Sampler sf) >>= f = Sampler $ \rs-> 
    let (x, rs'::Seed) = sf rs 
        nextProb = f x
    in case nextProb of
         Sampler g -> g rs'
         Samples xs -> primOneOf xs rs'

  (Samples xs) >>= f = Sampler $ \rs-> 
    let (x, rs'::Seed) = primOneOf xs rs
        nextProb = f x
    in case nextProb of
         Sampler g -> g rs'
         Samples ys -> primOneOf ys rs'

primOneOf :: [a] -> Seed -> (a, Seed)
primOneOf xs seed 
  = let (u, nextSeed) = randomDouble seed
        idx = floor $ realToFrac u * realToFrac (length xs )
    in (xs !! idx, nextSeed)

type DiscreteParams   = Vector Int
type ContinuousParams = Vector Double
type Parameters       = (DiscreteParams, ContinuousParams)

type LogObjective     = Parameters -> Double
type Gradient         = ContinuousParams -> ContinuousParams

type Particle         = (ContinuousParams, ContinuousParams)

type Transition t     = StateT (Chain t) Prob Parameters

-- | State of a Markov chain.  Note that the objective function itself is
--   included in the state, which allows the possibility of annealing schedules
--   and the like.
data Chain t = Chain {
    parameterSpacePosition :: Parameters
  , objectiveFunction      :: Target
  , objectiveValue         :: Double
  , tunables               :: t
  }

-- | A target to sample, consisting of a log objective function and possibly
--   its gradient.  The gradient is only taken with respect to continuous
--   parameters.
data Target = Target {
    logObjective :: Parameters -> Double
  , gradient     :: Maybe (ContinuousParams -> ContinuousParams)
  }

-- | Convenience constructor for targets and gradients.
createTargetWithGradient :: LogObjective -> Gradient -> Target
createTargetWithGradient f g = Target f (Just g)

-- | Convenience constructor for targets without gradients.
createTargetWithoutGradient :: LogObjective -> Target
createTargetWithoutGradient f = Target f Nothing

handleGradient :: Maybe t -> t
handleGradient Nothing  = error "handleGradient: no gradient provided"
handleGradient (Just g) = g

-- | The dual-averaging implementation of NUTS requires quite a few tuning
--   parameters, which are all held in this type.
data DualAveragingParameters = DualAveragingParameters {
    mAdapt    :: !Int
  , delta     :: !Double
  , mu        :: !Double
  , gammaP    :: !Double
  , tau0      :: !Double
  , kappa     :: !Double
  , daStep    :: !Double
  , daStepAvg :: !Double
  , daH       :: !Double
  } deriving (Eq, Show)

defaultDualAveragingParameters :: Double -> Int -> DualAveragingParameters
defaultDualAveragingParameters e burnInPeriod = DualAveragingParameters {
    mu        = log (10 * e)
  , delta     = 0.5
  , mAdapt    = burnInPeriod
  , gammaP    = 0.05
  , tau0      = 10
  , kappa     = 0.75
  , daStep    = e
  , daStepAvg = e
  , daH       = 0
  }


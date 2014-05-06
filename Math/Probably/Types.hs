{-# LANGUAGE ScopedTypeVariables #-}

module Math.Probably.Types where

import Control.Applicative
import Control.Monad.State.Strict
import Data.Map (Map)
import Numeric.LinearAlgebra
import System.Random.Mersenne.Pure64

type Seed = PureMT

data Prob a = Sampler {unSampler :: Seed -> (a, Seed) }
            | Samples [a]

instance Functor Prob where
    fmap f (Sampler sf) = Sampler $ \rs -> let (x,rs') = sf rs in
                                   (f x, rs')
    fmap f (Samples xs) = Samples $ map f xs
 
instance Applicative Prob where
    pure x = Sampler (\rs-> (x, rs))
    (Sampler sff) <*> (Sampler sfx) = Sampler $ \rs-> 
                        let (f ,rs') = sff rs 
                            (x, rs'') = sfx rs' 
                        in (f x, rs'')

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
         idx = floor $ (realToFrac u)*(realToFrac $ length xs )
     in (xs !! idx, nextSeed)

type Transition a = StateT (Chain a) Prob (Vector a)

data Chain a = Chain {
    parameterSpacePosition :: Vector a
  , objectiveFunction      :: Target a
  , objectiveValue         :: Double
  , optionalInformation    :: Tunables
  }

data Target a = Target {
    logObjective :: Vector a -> Double
  , gradient     :: Maybe (Vector a -> Vector a)
  }

createTargetWithGradient
  :: (Vector a -> Double)
  -> (Vector a -> Vector a)
  -> Target a
createTargetWithGradient f g = Target f (Just g)

createTargetWithoutGradient :: (Vector a -> Double) -> Target a
createTargetWithoutGradient f = Target f Nothing

handleGradient :: Maybe t -> t
handleGradient Nothing  = error "handleGradient: no gradient provided"
handleGradient (Just g) = g

type Tunables = Map Algorithm Tunable

data Tunable = 
    TDouble Double
  | TInt Int
  | TPair (Tunable, Tunable)
  deriving (Eq, Show)

data Algorithm =
    MH
  | Slice
  | MALA
  | HMC
  | NUTS
  deriving (Eq, Ord, Show)

{- |

This module defines the monad of sampling functions. See

Park, Pfenning and Thrun (2005) A probabilistic language based upon sampling
functions, Principles of Programming Languages 2005

Sampling functions allow the composition of both discrete and continuous
probability distributions.

The implementation and interface are similar to those in the random-fu,
monte-carlo and monad-mersenne-random packages.

Example -- a biased coin:

@
data Throw = Head | Tail

throw bias = do
  b <- bernoulli bias
  return $ if b then Head else Tail

tenThrowsCrooked = replicateM 10 $ throw 0.3

countHeads = do
   throws <- tenThrowsCrooked
   return $ length [ () | Head <- throws]

main = do
 print =<< sampleIO tenThrowsCrooked
 print =<< eval ((\<4) \`fmap\` countHeads)
@

-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module Math.Probably.Sampler where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Ord
import qualified Math.Probably.PDF as PDF
import Math.Probably.Types
import Numeric.LinearAlgebra hiding (find)
import System.Environment
import System.Random.Mersenne.Pure64

-- | given a seed, return an infinite list of draws from sampling function
runProb :: Seed -> Prob a -> [a]
runProb pmt s@(Sampler sf)
   = let (x, pmt') = sf pmt
     in x : runProb pmt' s
runProb _ (Samples xs) = xs

-- | given a seed, return an infinite list of draws from sampling function
runProbOne :: Seed -> Prob a -> (a,Seed)
runProbOne pmt (Sampler sf)
   = sf pmt
runProbOne pmt (Samples xs) = primOneOf xs pmt



-- | Get a seed
getSeedIO :: IO Seed
getSeedIO = do
   args <- getArgs
   case mapMaybe (stripPrefix "--seed=") args of
      [] ->  newPureMT
      sdStr:_ -> return $ pureMT $ read sdStr

-- | Return an infinite list of draws from sampling function in the IO monad
runProbIO :: Prob a -> IO [a]
runProbIO s =
   fmap (`runProb` s) $ getSeedIO

-- | Return a singe draw from sampling function
sampleIO :: Prob a -> IO a
sampleIO s = head `fmap` runProbIO s

-- | Return a list of n draws from sampling function
sampleNIO :: Int -> Prob a -> IO [a]
sampleNIO n s = take n `fmap` runProbIO s

-- | Estimate the probability that a hypothesis is true (in the IO monad)
eval :: Prob Bool -> Prob Double
eval s = do
  bs <- replicateM 1000 s
  return $ realToFrac (length (filter id bs)) / 1000



{-mu :: Vector Double
sigma :: Matrix Double

mystery = -1
mu = fromList [0,0,0]
sigma =  (3><3) [ 1,    1,       0,
                  1,    1,     mystery,
                  0,  mystery,   1]


samIt = sampleNIO 2 $ multiNormal mu sigma
 -}
-- | The joint distribution of two independent distributions
joint :: Prob a -> Prob b -> Prob (a,b)
joint sf1 sf2 = liftM2 (,) sf1 sf2

-- | The joint distribution of two distributions where one depends on the other
jointConditional :: Prob a -> (a-> Prob b) -> Prob (a,b)
jointConditional sf1 condsf
    = do x <- sf1
         y <- condsf x
         return (x,y)

--replicateM :: Monad m => Int -> m a -> m [a]
--replicateM n ma = forM [1..n] $ const ma


-- * Uniform distributions

-- | The unit interval U(0,1)
unit :: Prob Double
unit = Sampler randomDouble

-- | for x and y, the uniform distribution between x and y
uniform :: (Fractional a) => a -> a -> Prob a
uniform a b = (\x->(realToFrac x)*(b-a)+a) `fmap` unit

-- | useful for data display
jitter :: Double -> [Int] -> Prob [Double]
jitter j = mapM $ \i-> let x = realToFrac i in uniform (x-j) (x+j)

-- * Normally distributed sampling function

--http://en.wikipedia.org/wiki/Box-Muller_transform
-- | The univariate gaussian (normal) distribution defined by mean and standard deviation

unormal ::  Prob Double
unormal = do
   u1 <- unit
   u2 <- unit
   return (sqrt ((0.0-2.0) * log u1) * cos(2.0 * pi * u2))

normal :: Double -> Double -> Prob Double
normal mean variance = do
   u <- unormal
   return (u * (sqrt variance) + mean)



normalMany :: [(Double,Double)] -> Prob [Double]
normalMany means_vars = do gus <- normalManyUnit (length means_vars)
                           return $ map f $ zip gus means_vars
    where f (gu, (mean, var)) = gu*(sqrt var)+mean


normalManyUnit :: Int -> Prob [Double]
normalManyUnit 0 = return []
normalManyUnit n | odd n = liftM2 (:) unormal (normalManyUnit (n-1))
                 | otherwise = do us <- forM [1..n] $ const $ unit
                                  return $ gaussTwoAtATime $ map realToFrac us
  where
    gaussTwoAtATime :: Floating a =>  [a] -> [a]
    gaussTwoAtATime (u1:u2:rest) = sqrt(-2*log(u1))*cos(2*pi*u2) : sqrt(-2*log(u1))*sin(2*pi*u2) : gaussTwoAtATime rest
    gaussTwoAtATime _ = []


-- | Multivariate normal distribution
multiNormal :: Vector Double -> Matrix Double -> Prob (Vector Double)
multiNormal mu sigma =
  let c =   cholSH sigma
      a = trans c
      k = dim mu
  in do z <- fromList `fmap` normalManyUnit k
--        return $ mu + (head $ toColumns $ a*asRow z)
        let c = asColumn z
        let r = asRow z
        return $ (mu + (head $ toColumns $ a `multiply` asColumn z))

multiNormalByChol :: Vector Double -> Matrix Double -> Prob (Vector Double)
multiNormalByChol mu cholSigma =
  let a = trans $ cholSigma
      k = dim mu
  in do z <- fromList `fmap` normalManyUnit k
--        return $ mu + (head $ toColumns $ a*asRow z)
        let c = asColumn z
        let r = asRow z
        return $ (mu + (head $ toColumns $ a `multiply` asColumn z))

multiNormalIndep  :: Vector Double -> Vector Double -> Prob (Vector Double)
multiNormalIndep vars mus = do
   let k = dim mus
   gs <- normalManyUnit k
   return $ fromList $ zipWith3 (\var mu g -> g*sqrt(var) + mu) (toList vars) (toList mus) gs



--http://en.wikipedia.org/wiki/Log-normal_distribution#Generating_log-normally-distributed_random_variates

-- | log-normal distribution <http://en.wikipedia.org/wiki/Log-normal_distribution>
logNormal :: Double -> Double -> Prob Double
logNormal m var =
    fmap exp $  normal m var

-- * Other distribution

-- | Bernoulli distribution. Returns a Boolean that is 'True' with probability 'p'
bernoulli :: Double -> Prob Bool
bernoulli p = (<p) `fmap` unit


discrete :: [(Double,a)] -> Prob a
discrete weightedSamples =
   let sumWeights = sum $ map fst weightedSamples
       cummWeightedSamples = scanl (\(csum,_) (w,x) -> (csum+w,x)) (0,undefined) $ sortBy (comparing fst) weightedSamples
   in do u <- unit
         return . snd . fromJust $ find ((>=u*sumWeights) . fst) cummWeightedSamples


-- primOneOf :: [a] -> Seed -> (a, Seed)
-- primOneOf xs seed
--    = let (u, nextSeed) = randomDouble seed
--          idx = floor $ (realToFrac u)*(realToFrac $ length xs )
--      in (xs !! idx, nextSeed)

oneOf :: [a] -> Prob a
oneOf xs = do idx <- floor `fmap` uniform (0::Double) (realToFrac $ length xs )
              return $ xs !! idx

nOf :: Int -> [a] -> Prob [a]
nOf n xs = sequence $ replicate n $ oneOf xs


--sampling without replacement. Terrible performance
nDistinctOf :: Int ->  [a] -> Prob [a]
nDistinctOf wantN xs = do
  let haveN = length xs
      select ys | length ys == wantN = return ys
                | otherwise = do
                    y <- floor `fmap` uniform (0::Double) (realToFrac $ length xs )
                    if y `elem` ys
                       then select ys
                       else select (y:ys)
  ixs <- select []
  return $ map (xs!!) ixs

withoutReplacementFrom :: Eq a => Int -> Prob a -> Prob [a]
withoutReplacementFrom wantN p = select [] where
  select ys | length ys == wantN = return ys
            | otherwise = do
                y <- p
                if y `elem` ys
                  then select ys
                  else select (y:ys)


-- | Bayesian inference from likelihood and prior using rejection sampling.
bayesRejection :: (PDF.PDF a) -> Double -> Prob a -> Prob a
bayesRejection p c q = bayes
    where bayes = do x <- q
                     u <- unit
                     if u < p x / c
                        then return x
                        else bayes

--poisson ::  ::  Double -> [Double] -> IO Double
-- | Exponential distribution
expDist rate =  (\u-> negate $ (log(1-u))/rate) `fmap` unit

poissonAux :: Double -> Int -> Double -> Prob Int
poissonAux bigl k p = if p>bigl
                         then do
                           u<- unit
                           poissonAux bigl (k+1) (p*u)
                         else return (k-1)

poisson :: Double -> (Prob Int)
poisson lam =  poissonAux (exp (-lam)) 0 1


-- | binomial distribution
binomial :: Int -> Double -> Prob Int
binomial n p = do
  bools <- forM [1..n] $ const $ fmap (<p) unit
  return $ length $ [t | t@True <- bools]

-- from random-fu
-- | Gamma distribution
gamma :: Double -> Double -> Prob Double
gamma a b
     | a < 1
    = do
        u <- unit
        x <- gamma (1 + a) b
        return (x * u ** recip a)
    | otherwise
    = go
        where
            d = a - (1 / 3)
            c = recip (3 * sqrt d) -- (1 / 3) / sqrt d

            go = do
                x <- unormal

                let cx = c * x
                    v = (1 + cx) ^ 3

                    x_2 = x * x
                    x_4 = x_2 * x_2

                if cx <= (-1)
                    then go
                    else do
                        u <- unit

                        if         u < 1 - 0.0331 * x_4
                            || log u < 0.5 * x_2  + d * (1 - v + log v)
                            then return (b * d * v)
                            else go

-- | inverse gamma distribution
invGamma :: Double -> Double -> Prob Double
invGamma a b = recip `fmap` gamma a b

-- | beta distribution
beta :: Int -> Int -> Prob Double
beta a b =
    let gam n = do us <- forM [1..n] $ const unit
                   return $ log $ product us
    in do gama1 <- gamma (realToFrac a) 1

          gamb <- gamma (realToFrac b) 1
          return $ gama1/(gama1+gamb)

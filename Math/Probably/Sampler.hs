-----------------------------------------------------------------------------
{- |
This module defines the monad of sampling functions. See Park, Pfenning and Thrun:
A probabilistic language based upon sampling functions, Principles of programming languages 2005
 
Sampling functions allow the composition of both discrete and continuous 
probability distributions. 

The implementation and interface are similar to those in the random-fu, monte-carlo 
and monad-mersenne-random packages.

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

{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module Math.Probably.Sampler where

import Control.Monad
import Control.Applicative
import qualified Math.Probably.PDF as PDF
import Numeric.LinearAlgebra hiding (find)
import System.Random.Mersenne.Pure64
import System.Environment
import Data.List
import Data.Maybe
import Data.Ord
import Control.Spoon

--import Debug.Trace

type Seed = PureMT


data Sampler a = Sam {unSam :: Seed -> (a, Seed) }
               | Samples [a]

instance Functor Sampler where
    fmap f (Sam sf) = Sam $ \rs -> let (x,rs') = sf rs in
                                   (f x, rs')
    fmap f (Samples xs) = Samples $ map f xs
 
instance Applicative Sampler where
    pure x = Sam (\rs-> (x, rs))
    (Sam sff) <*> (Sam sfx) = Sam $ \rs-> let (f ,rs') = sff rs 
                                              (x, rs'') = sfx rs' in
                                          (f x, rs'')

instance Monad Sampler where
    return = pure
    (Sam sf) >>= f = Sam $ \rs-> let (x, rs'::Seed) = sf rs 
                                     nextProb = f x
                                 in case nextProb of
                                      Sam g -> g rs'
                                      Samples xs -> primOneOf xs rs'
    (Samples xs) >>= f = Sam $ \rs-> let (x, rs'::Seed) = primOneOf xs rs
                                         nextProb = f x
                                     in case nextProb of
                                          Sam g -> g rs'
                                          Samples ys -> primOneOf ys rs'

-- | given a seed, return an infinite list of draws from sampling function
runSampler :: Seed -> Sampler a -> [a]
runSampler pmt s@(Sam sf) 
   = let (x, pmt') = sf pmt
     in x : runSampler pmt' s
runSampler _ (Samples xs) = xs

-- | Get a seed
getSeedIO :: IO Seed
getSeedIO = do
   args <- getArgs
   case mapMaybe (stripPrefix "--seed=") args of 
      [] ->  newPureMT
      sdStr:_ -> return $ pureMT $ read sdStr

-- | Return an infinite list of draws from sampling function in the IO monad
runSamplerIO :: Sampler a -> IO [a]
runSamplerIO s = 
   fmap (`runSampler` s) $ getSeedIO

-- | Return a singe draw from sampling function
sampleIO :: Sampler a -> IO a
sampleIO s = head `fmap` runSamplerIO s

-- | Return a list of n draws from sampling function
sampleNIO :: Int -> Sampler a -> IO [a]
sampleNIO n s = take n `fmap` runSamplerIO s

-- | Estimate the probability that a hypothesis is true (in the IO monad)
eval :: Sampler Bool -> Sampler Double
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
joint :: Sampler a -> Sampler b -> Sampler (a,b)
joint sf1 sf2 = liftM2 (,) sf1 sf2

-- | The joint distribution of two distributions where one depends on the other
jointConditional :: Sampler a -> (a-> Sampler b) -> Sampler (a,b)
jointConditional sf1 condsf 
    = do x <- sf1
         y <- condsf x
         return (x,y)

--replicateM :: Monad m => Int -> m a -> m [a]
--replicateM n ma = forM [1..n] $ const ma

                            
-- * Uniform distributions
     
-- | The unit interval U(0,1)
unitSample :: Sampler Double
unitSample = Sam randomDouble 

-- | for x and y, the uniform distribution between x and y 
uniform :: (Fractional a) => a -> a -> Sampler a
uniform a b = (\x->(realToFrac x)*(b-a)+a) `fmap` unitSample
                
-- * Normally distributed sampling function

--http://en.wikipedia.org/wiki/Box-Muller_transform
-- | The univariate gaussian (normal) distribution defined by mean and standard deviation
gauss :: (Floating b) => b -> b -> Sampler b
gauss m sd = 
    do (u1,u2) <- (mapPair realToFrac) `fmap` joint unitSample unitSample
       return $ sqrt(-2*log(u1))*cos(2*pi*u2)*sd+m
  where mapPair f (x,y) = (f x, f y)

-- | Gaussians specialised for doubles
gaussD :: Double -> Double -> Sampler Double
gaussD m sd = 
    do (u1,u2) <- joint unitSample unitSample
       return $ sqrt(-2*log(u1))*cos(2*pi*u2)*sd+m


gaussMany :: Floating b => [(b,b)] -> Sampler [b]
gaussMany means_sds = do gus <- gaussManyUnit (length means_sds)
                         return $ map f $ zip gus means_sds
    where f (gu, (mean, sd)) = gu*sd+mean

gaussManyD :: [(Double,Double)] -> Sampler [Double]
gaussManyD means_sds = do gus <- gaussManyUnitD (length means_sds)
                          return $ zipWith f gus means_sds
    where f gu (mean, sd) = gu*sd+mean

gaussManyUnit :: Floating b => Int -> Sampler [b]
gaussManyUnit 0 = return []
gaussManyUnit n | odd n = liftM2 (:) (gauss 0 1) (gaussManyUnit (n-1))
                | otherwise = do us <- forM [1..n] $ const $ unitSample
                                 return $ gaussTwoAtATime $ map realToFrac us
  where 
    gaussTwoAtATime :: Floating a =>  [a] -> [a]
    gaussTwoAtATime (u1:u2:rest) = sqrt(-2*log(u1))*cos(2*pi*u2) : sqrt(-2*log(u1))*sin(2*pi*u2) : gaussTwoAtATime rest
    gaussTwoAtATime _ = []

gaussManyUnitD :: Int -> Sampler [Double]
gaussManyUnitD 0 = return []
gaussManyUnitD n | odd n = liftM2 (:) (gauss 0 1) (gaussManyUnit (n-1))
                | otherwise = do us <- forM [1..n] $ const $ unitSample
                                 return $ gaussTwoAtATimeD us
 where
   gaussTwoAtATimeD :: [Double] -> [Double]
   gaussTwoAtATimeD (u1:u2:rest) = sqrt(-2*log(u1))*cos(2*pi*u2) : sqrt(-2*log(u1))*sin(2*pi*u2) : gaussTwoAtATimeD rest
   gaussTwoAtATimeD _ = []



-- | Multivariate normal distribution
multiNormal :: Vector Double -> Matrix Double -> Sampler (Vector Double)
multiNormal mu sigma =
  let c =   cholSH sigma
      a = trans c
      k = dim mu
  in do z <- fromList `fmap` gaussManyUnitD k
--        return $ mu + (head $ toColumns $ a*asRow z)
        let c = asColumn z
        let r = asRow z
        return $ (mu + (head $ toColumns $ a `multiply` asColumn z))

multiNormalByChol :: Vector Double -> Matrix Double -> Sampler (Vector Double)
multiNormalByChol mu cholSigma =
  let a = trans $ cholSigma
      k = dim mu
  in do z <- fromList `fmap` gaussManyUnitD k
--        return $ mu + (head $ toColumns $ a*asRow z)
        let c = asColumn z
        let r = asRow z
        return $ (mu + (head $ toColumns $ a `multiply` asColumn z))

multiNormalIndep  :: Vector Double -> Vector Double -> Sampler (Vector Double)
multiNormalIndep vars mus = do
   let k = dim mus
   gs <- gaussManyUnitD k
   return $ fromList $ zipWith3 (\var mu g -> g*sqrt(var) + mu) (toList vars) (toList mus) gs



--http://en.wikipedia.org/wiki/Log-normal_distribution#Generating_log-normally-distributed_random_variates

-- | log-normal distribution <http://en.wikipedia.org/wiki/Log-normal_distribution>
logNormal :: (Floating b) => b -> b -> Sampler b
logNormal m sd = 
    do n <- gauss 0 1
       return $ exp $ m + sd * n


-- * Other distribution

-- | Bernoulli distribution. Returns a Boolean that is 'True' with probability 'p'
bernoulli :: Double -> Sampler Bool
bernoulli p = (<p) `fmap` unitSample 


discrete :: [(Double,a)] -> Sampler a
discrete weightedSamples = 
   let sumWeights = sum $ map fst weightedSamples
       cummWeightedSamples = scanl (\(csum,_) (w,x) -> (csum+w,x)) (0,undefined) $ sortBy (comparing fst) weightedSamples
   in do u <- unitSample
         return . snd . fromJust $ find ((>=u*sumWeights) . fst) cummWeightedSamples


primOneOf :: [a] -> Seed -> (a, Seed)
primOneOf xs seed 
   = let (u, nextSeed) = randomDouble seed
         idx = floor $ (realToFrac u)*(realToFrac $ length xs )
     in (xs !! idx, nextSeed)

oneOf :: [a] -> Sampler a
oneOf xs = do idx <- floor `fmap` uniform (0::Double) (realToFrac $ length xs )
              return $ xs !! idx

nOf :: Int -> [a] -> Sampler [a]
nOf n xs = sequence $ replicate n $ oneOf xs

{-main = do 
  rnds <- take 1000 `fmap` runSamplerSysRan (oneOf [1,2,3])
  let diff = sort $ nub rnds
  print $ map (\x->(x, length $ filter (==x) rnds )) $ diff -}

-- | Bayesian inference from likelihood and prior using rejection sampling. 
bayesRejection :: (PDF.PDF a) -> Double -> Sampler a -> Sampler a
bayesRejection p c q = bayes
    where bayes = do x <- q
                     u <- unitSample
                     if u < p x / c 
                        then return x
                        else bayes      

                     
{-
expectation :: Fractional a =>  Int -> Sampler a -> IO a
expectation n sf = 
    (mean . take n) `fmap` runSamplerIO sf
 
expectSD :: Floating a =>  Int -> Sampler a -> IO (a,a)
expectSD n sf = 
    (meanSD . take n) `fmap` runSamplerIO sf
-}                 

--mapPair :: (a->b) -> (a,a) -> (b,b)


{-
--http://cgi.cse.unsw.edu.au/~dons/blog/2008/05/16#fast
mean :: Fractional a =>  [a] -> a
mean = go 0 0
        where
            -- go ::  -> Int -> [Double] -> Double
            go s n []     = s / fromIntegral n
            go !s !n (x:xs) = go (s+x) (n+1) xs

meanSD :: Floating a => [a] -> (a,a)
meanSD = go 0 0 0
    where go sq s n [] = let len = fromIntegral n in
                         (s/len, (recip len)*sqrt (len*sq-s*s))
          go !sq !s !n (x:xs) = go (sq+x*x) (s+x) (n+1) xs



test_mean = mean [0..1e8]
test_meanVar = meanSD [0..1e8]

main = do u <- expectSD 1000000 $ gauss 0 1
          print u

-}


--poisson ::  ::  Double -> [Double] -> IO Double
-- | Exponential distribution
expDist rate =  (\u-> negate $ (log(1-u))/rate) `fmap` unitSample

-- | Homogeneous poisson process defined by rate and duration
poissonMany :: Double -> Double -> Sampler [Double]
poissonMany rate tmax = aux 0 
    where aux last = do
            next <- (+last) `fmap` expDist rate
            if next > tmax
               then return []
               else liftM2 (:) (return next) $ aux next

-- | binomial distribution 
binomial :: Int -> Double -> Sampler Int
binomial n p = do
  bools <- forM [1..n] $ const $ fmap (<p) unitSample
  return $ length $ [t | t@True <- bools]
  
-- from random-fu
-- | Gamma distribution
gamma :: Double -> Double -> Sampler Double
gamma a b 
     | a < 1 
    = do
        u <- unitSample
        x <- gamma (1 + a) b
        return (x * u ** recip a)
    | otherwise
    = go
        where
            d = a - (1 / 3)
            c = recip (3 * sqrt d) -- (1 / 3) / sqrt d
            
            go = do
                x <- gaussD 0 1
                
                let cx = c * x
                    v = (1 + cx) ^ 3
                    
                    x_2 = x * x
                    x_4 = x_2 * x_2
                
                if cx <= (-1)
                    then go
                    else do
                        u <- unitSample
                        
                        if         u < 1 - 0.0331 * x_4
                            || log u < 0.5 * x_2  + d * (1 - v + log v)
                            then return (b * d * v)
                            else go

-- | inverse gamma distribution
invGamma :: Double -> Double -> Sampler Double
invGamma a b = recip `fmap` gamma a b

--http://en.wikipedia.org/wiki/Multivariate_normal_distribution#Drawing_values_from_the_distribution
--multiNormal :: Vector Double -> Matrix Double -> Sampler (Vector Double)

--http://www.xycoon.com/beta_randomnumbers.htm
-- | beta distribution
beta :: Int -> Int -> Sampler Double
beta a b = 
    let gam n = do us <- forM [1..n] $ const unitSample
                   return $ log $ product us
    in do gama1 <- gamma (realToFrac a) 1
--          gama2 <- gamma (realToFrac a) 1


          gamb <- gamma (realToFrac b) 1
          return $ gama1/(gama1+gamb)

tbeta = sampleNIO 100 $ beta 1 1
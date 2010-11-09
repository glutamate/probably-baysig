{-# LANGUAGE BangPatterns #-}

module Math.Probably.Sampler where

import System.Random.Mersenne
import Control.Monad
import Control.Applicative
import Data.Array.Vector
import qualified Math.Probably.PDF as PDF
import qualified System.Random as SR
import Data.List
import Numeric.LinearAlgebra


newtype Sampler a = Sam {unSam :: [Double] -> (a, [Double]) }


unitSample :: Sampler Double
unitSample = Sam $ \(r:rs) -> (r,rs)

instance Functor Sampler where
    fmap f (Sam sf) = Sam $ \rs -> let (x,rs') = sf rs in
                                   (f x, rs')

instance Applicative Sampler where
    pure x = Sam (\rs-> (x, rs))
    (Sam sff) <*> (Sam sfx) = Sam $ \rs-> let (f ,rs') = sff rs 
                                              (x, rs'') = sfx rs' in
                                          (f x, rs'')

instance Monad Sampler where
    return = pure
    (Sam sf) >>= f = Sam $ \rs-> let (x, rs') = sf rs in
                               (unSam $ f x) rs'

(.==.), (./=.) :: (Eq a, Applicative m) => m a -> m a -> m Bool
(.==.) = liftA2 (==)
(./=.) = liftA2 (/=)

compareA :: (Ord a, Applicative m) => m a -> m a -> m Ordering
compareA = liftA2 (compare) 

(.<.), (.>.), (.>=.), (.<=.):: (Ord a, Applicative m) => m a -> m a -> m Bool
(.<.) = liftA2 (<)
(.>.) = liftA2 (>)
(.>=.) = liftA2 (>=)
(.<=.) = liftA2 (<=)

--conor mcbride haskell-cafe 04 may 09
if_ :: Monad m => m Bool -> m t -> m t -> m t
if_ test yes no = do
  b <- test
  if b then yes else no



joint :: Sampler a -> Sampler b -> Sampler (a,b)
joint sf1 sf2 = liftM2 (,) sf1 sf2

jointConditional :: Sampler a -> (a-> Sampler b) -> Sampler (a,b)
jointConditional sf1 condsf 
    = do x <- sf1
         y <- condsf x
         return (x,y)
                                 

uniform :: (Fractional a) => a -> a -> Sampler a
uniform a b = (\x->(realToFrac x)*(b-a)+a) `fmap` unitSample
                
--http://en.wikipedia.org/wiki/Box-Muller_transform
gauss :: (Floating b) => b -> b -> Sampler b
gauss m sd = 
    do (u1,u2) <- (mapPair realToFrac) `fmap` joint unitSample unitSample
       return $ sqrt(-2*log(u1))*cos(2*pi*u2)*sd+m

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

gaussManyUnitD :: Int -> Sampler [Double]
gaussManyUnitD 0 = return []
gaussManyUnitD n | odd n = liftM2 (:) (gauss 0 1) (gaussManyUnit (n-1))
                | otherwise = do us <- forM [1..n] $ const $ unitSample
                                 return $ gaussTwoAtATimeD us
                                 

gaussTwoAtATime :: Floating a =>  [a] -> [a]
gaussTwoAtATime (u1:u2:rest) = sqrt(-2*log(u1))*cos(2*pi*u2) : sqrt(-2*log(u1))*sin(2*pi*u2) : gaussTwoAtATime rest
gaussTwoAtATime _ = []

gaussTwoAtATimeD :: [Double] -> [Double]
gaussTwoAtATimeD (u1:u2:rest) = sqrt(-2*log(u1))*cos(2*pi*u2) : sqrt(-2*log(u1))*sin(2*pi*u2) : gaussTwoAtATime rest
gaussTwoAtATimeD _ = []

bernoulli :: Double -> Sampler Bool
bernoulli p = (<p) `fmap` unitSample 

--http://en.wikipedia.org/wiki/Log-normal_distribution#Generating_log-normally-distributed_random_variates
logNormal :: (Floating b) => b -> b -> Sampler b
logNormal m sd = 
    do n <- gauss 0 1
       return $ exp $ m + sd * n

oneOf :: [a] -> Sampler a
oneOf xs = do idx <- floor `fmap` uniform (0::Double) (realToFrac $ length xs )
              return $ xs !! idx

nOf :: Int -> [a] -> Sampler [a]
nOf n xs = sequence $ replicate n $ oneOf xs

{-main = do 
  rnds <- take 1000 `fmap` runSamplerSysRan (oneOf [1,2,3])
  let diff = sort $ nub rnds
  print $ map (\x->(x, length $ filter (==x) rnds )) $ diff -}

bayesRejection :: (PDF.PDF a) -> Double -> Sampler a -> Sampler a
bayesRejection p c q = bayes
    where bayes = do x <- q
                     u <- unitSample
                     if u < p x / c 
                        then return x
                        else bayes      

runSampler :: [Double] -> Sampler a -> [a]
runSampler rs sf = let (x,rs') = (unSam sf) rs in x:runSampler rs' sf

runSamplerIO :: Sampler a -> IO [a]
runSamplerIO sf = do rnds <- randoms =<< getStdGen 
                     return $ runSampler rnds sf

runSamplerSysRan :: Sampler a -> IO [a]
runSamplerSysRan sf = do 
  rnds <- SR.randoms `fmap` SR.getStdGen
  return $ runSampler rnds sf
{-
expectation :: Fractional a =>  Int -> Sampler a -> IO a
expectation n sf = 
    (mean . take n) `fmap` runSamplerIO sf
 
expectSD :: Floating a =>  Int -> Sampler a -> IO (a,a)
expectSD n sf = 
    (meanSD . take n) `fmap` runSamplerIO sf
-}                 

mapPair :: (a->b) -> (a,a) -> (b,b)
mapPair f (x,y) = (f x, f y)

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
poisson rate =  (\u-> negate $ (log(1-u))/rate) `fmap` unitSample
poissonMany :: Double -> Double -> Sampler [Double]
poissonMany rate tmax = aux 0 
    where aux last = do
            next <- (+last) `fmap` poisson rate
            if next > tmax
               then return []
               else liftM2 (:) (return next) $ aux next

binomial :: Int -> Double -> Sampler Int
binomial n p = do
  bools <- forM [1..n] $ const $ fmap (<p) unitSample
  return $ length $ [t | t@True <- bools]
  
-- from random-fu
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

invGamma :: Double -> Double -> Sampler Double
invGamma a b = recip `fmap` gamma a b

--http://en.wikipedia.org/wiki/Multivariate_normal_distribution#Drawing_values_from_the_distribution
--multiNormal :: Vector Double -> Matrix Double -> Sampler (Vector Double)
multiNormal mu sigma =
  let a = chol sigma
      k = dim mu
  in do z <- fromList `fmap` gaussManyUnitD k
--        return $ mu + (head $ toColumns $ a*asRow z)
        let c = asColumn z
        let r = asRow z
        return $ (mu + (head $ toColumns $ a `multiply` asColumn z))

--http://www.xycoon.com/beta_randomnumbers.htm
beta :: Int -> Int -> Sampler Double
beta a b = 
    let gam n = do us <- forM [1..n] $ const unitSample
                   return $ log $ product us
    in do gama1 <- gam a
          gama2 <- gam a
          gamb <- gam b
          return $ gama1/(gama2+gamb)
{-# LANGUAGE BangPatterns #-}

module Math.Probably.Sampler where

import System.Random.Mersenne
import Control.Monad
import Control.Applicative
import Data.Array.Vector

newtype Sampler a = SF {unSF :: [Double] -> (a, [Double]) }

unitSample :: Sampler Double
unitSample = SF $ \(r:rs) -> (r,rs)

instance Functor Sampler where
    fmap f (SF sf) = SF $ \rs -> let (x,rs') = sf rs in
                                 (f x, rs')

instance Applicative Sampler where
    pure x = SF (\rs-> (x, rs))
    (SF sff) <*> (SF sfx) = SF $ \rs-> let (f ,rs') = sff rs 
                                           (x, rs'') = sfx rs' in
                                       (f x, rs'')

instance Monad Sampler where
    return = pure
    (SF sf) >>= f = SF $ \rs-> let (x, rs') = sf rs in
                               (unSF $ f x) rs'

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


bernoulli :: Double -> Sampler Bool
bernoulli p = (<p) `fmap` unitSample 


oneOf :: [a] -> Sampler a
oneOf xs = do idx <- floor `fmap` uniform (0::Double) (realToFrac $ length xs -1)
              return $ xs !! idx

bayesRejection :: (a->Double) -> Double -> Sampler a -> Sampler a
bayesRejection p c q = bayes
    where bayes = do x <- q
                     u <- unitSample
                     if u < p x / c 
                        then return x
                        else bayes      

runSampler :: [Double] -> Sampler a -> [a]
runSampler rs sf = let (x,rs') = (unSF sf) rs in x:runSampler rs' sf

runSamplerIO :: Sampler a -> IO [a]
runSamplerIO sf = do rnds <- randoms =<< getStdGen 
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

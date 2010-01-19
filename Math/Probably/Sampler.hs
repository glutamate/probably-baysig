{-# LANGUAGE BangPatterns #-}

module Math.Probably.Sampler where

import System.Random.Mersenne
import Control.Monad
import Control.Applicative
import Data.Array.Vector
import qualified Math.Probably.PDF as PDF
import qualified System.Random as SR
import Data.List

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
                          return $ map f $ zip gus means_sds
    where f (gu, (mean, sd)) = gu*sd+mean

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



oneOf :: [a] -> Sampler a
oneOf xs = do idx <- floor `fmap` uniform (0::Double) (realToFrac $ length xs )
              return $ xs !! idx

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
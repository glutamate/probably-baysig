{-# OPTIONS -fbang-patterns #-}

module SamFun where

import System.Random
import Control.Monad
import Control.Applicative
import Data.Array.Vector

newtype SamFun a = SF {unSF :: [Double] -> (a, [Double]) }

unitSample :: SamFun Double
unitSample = SF $ \(r:rs) -> (r,rs)

instance Functor SamFun where
    fmap f (SF sf) = SF $ \rs -> let (x,rs') = sf rs in
                                 (f x, rs')

instance Applicative SamFun where
    pure x = SF (\rs-> (x, rs))
    (SF sff) <*> (SF sfx) = SF $ \rs-> let (f ,rs') = sff rs 
                                           (x, rs'') = sfx rs' in
                                       (f x, rs'')

instance Monad SamFun where
    return = pure
    (SF sf) >>= f = SF $ \rs-> let (x, rs') = sf rs in
                               (unSF $ f x) rs'

joint :: SamFun a -> SamFun b -> SamFun (a,b)
joint sf1 sf2 = liftM2 (,) sf1 sf2

jointConditional :: SamFun a -> (a-> SamFun b) -> SamFun (a,b)
jointConditional sf1 condsf 
    = do x <- sf1
         y <- condsf x
         return (x,y)
                                 

uniform :: (Fractional a) => a -> a -> SamFun a
uniform a b = (\x->(realToFrac x)*(b-a)+a) `fmap` unitSample
                
--http://en.wikipedia.org/wiki/Box-Muller_transform
gauss :: (Floating b) => b -> b -> SamFun b
gauss m sd = 
    do (u1,u2) <- (mapPair realToFrac) `fmap` joint unitSample unitSample
       return $ sqrt(-2*log(u1))*cos(2*pi*u2)*sd+m


bernoulli :: Double -> SamFun Bool
bernoulli p = (<p) `fmap` unitSample 


oneOf :: [a] -> SamFun a
oneOf xs = do idx <- floor `fmap` uniform (0::Double) (realToFrac $ length xs -1)
              return $ xs !! idx

runSamFun :: [Double] -> SamFun a -> [a]
runSamFun rs sf = let (x,rs') = (unSF sf) rs in x:runSamFun rs' sf

expectation :: Fractional a =>  Int -> SamFun a -> IO a
expectation n sf = 
    do rnds <- randoms `fmap` getStdGen 
       return . mean $ runSamFun rnds sf
                 

mapPair :: (a->b) -> (a,a) -> (b,b)
mapPair f (x,y) = (f x, f y)


--http://cgi.cse.unsw.edu.au/~dons/blog/2008/05/16#fast
mean :: Fractional a =>  [a] -> a
mean = go 0 0
        where
            -- go ::  -> Int -> [Double] -> Double
            go s n []     = s / fromIntegral n
            go !s !n (x:xs) = go (s+x) (n+1) xs


{-mean :: Fractional a => [a] -> a
mean xs = (fstS fld) / (sndS fld)
    where fld = foldl (\(s :*: n) v -> (s+v :*: n+1)) (0 :*: 0) xs -}

meanVar :: Floating a => [a] -> (a,a)
meanVar = go 0 0 0
    where go sq s n [] = let len = fromIntegral n in
                         (s/len, (recip len)*sqrt (len*sq-s*s))
          go !sq !s !n (x:xs) = go (sq+x*x) (s+x) (n+1) xs

{-meanVar xs = (sum / len, (recip len)*sqrt (len*sumsq-sum*sum))
    where bigfold = foldl (\(sumsqr :*: sumv :*: n) v -> (sumsqr+v^2 :*: sumv+v :*: n+1)) (0 :*:0 :*:0) xs
	  sumsq = fst3 bigfold
	  sum = snd3 bigfold
	  len = realToFrac $ trd3 bigfold
          fst3 (x:*:y:*:z) = x
          snd3 (x:*:y:*:z) = y
          trd3 (x:*:y:*:z) = z-}

-- sum xs / (realToFrac $ length xs) 

test_mean = mean [0..1e8]
test_meanVar = meanVar [0..1e8]
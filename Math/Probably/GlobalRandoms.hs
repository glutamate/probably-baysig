module Math.Probably.GlobalRandoms where

import Data.IORef
import System.IO.Unsafe
import Math.Probably.Sampler

import Numeric.LinearAlgebra


{-# NOINLINE globalSeed #-}
globalSeed :: IORef Seed
globalSeed = unsafePerformIO $ getSeedIO >>= newIORef
                  
{-# NOINLINE sampleN #-}
sampleN :: Int -> Sampler a -> [a] 
sampleN n = unsafePerformIO . fmap (take n) . runSamplerIO 

{-  withGlobalRnds (\rs -> sam n rs sf [])
    where sam 0 rs _ xs          = (xs, rs)
          sam n rs s@(Sam sf) xs = let (x, rs') = sf rs 
                                   in sam (n-1) rs' s (x:xs) -}

{-sampleNV :: Int -> Sampler a -> Vector a
sampleNV n sf = 
  withGlobalRnds (\rs -> sam n rs sf [])
    where sam 0 rs _ xs          = (xs, rs)
          sam n rs s@(Sam sf) xs = let (x, rs') = sf rs 
                                   in sam (n-1) rs' s (x:xs) -}


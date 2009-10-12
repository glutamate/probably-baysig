module Math.Probably.GlobalRandoms where

import System.Random.Mersenne
import Data.IORef
import System.IO.Unsafe
import Math.Probably.Sampler

{-# NOINLINE globalRandoms #-}
globalRandoms :: IORef [Double]
globalRandoms = unsafePerformIO $ getStdGen >>= randoms >>= newIORef
                  
{-# NOINLINE rnds #-}
rnds :: Int -> [Double]
rnds n = unsafePerformIO $ do
           (xs, ys) <- splitAt n `fmap` readIORef globalRandoms
           writeIORef globalRandoms ys
           return xs

{-# NOINLINE rnd #-}
rnd :: Double 
rnd = head $ rnds 1

{-# NOINLINE withGlobalRnds #-}
withGlobalRnds :: ([Double] -> (a,[Double])) -> a
withGlobalRnds f = unsafePerformIO $ do
                     rnds <- readIORef globalRandoms
                     let (x, ys) = f rnds
                     writeIORef globalRandoms ys
                     return x

{-# NOINLINE sampleN #-}
sampleN :: Int -> Sampler a -> [a]
sampleN n sf = 
  withGlobalRnds (\rs -> sam n rs sf [])
    where sam 0 rs _ xs          = (xs, rs)
          sam n rs s@(Sam sf) xs = let (x, rs') = sf rs 
                                   in sam (n-1) rs' s (x:xs)
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Strategy.Slice where

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.ST
import Data.Vector.Storable (Vector, Storable)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as V hiding (length)
import Math.Probably.Sampler
import Math.Probably.Types

slice :: Double -> Transition Double
slice e = do
  Chain position target _ _ <- get
  let n = V.length position
  forM_ [0..n - 1] $ \j -> do
    Chain q _ _ store <- get
    height  <- liftM log $ lift $ uniform 0 (exp $ logObjective target q)
    let bracket = findBracket (logObjective target) j e height q
    next    <- lift $ rejection (logObjective target) j bracket height q
    put $ Chain next target (logObjective target next) store
  gets parameterSpacePosition

findBracket :: (Num b, Ord a, Storable b)
  => (Vector b -> a)
  -> Int
  -> b
  -> a
  -> Vector b
  -> (b, b)
findBracket f j step height xs = runST (go step xs xs) where
  go !e !bl !br
    | f bl <  height && f br <  height =
        return (bl `V.unsafeIndex` j , br `V.unsafeIndex` j)
    | f bl <  height && f br >= height = do
        br0 <- expandBracketRight j e br
        go (2 * e) bl br0 
    | f bl >= height && f br <  height = do
        bl0 <- expandBracketLeft j e bl
        go (2 * e) bl0 br
    | otherwise = do
        bl0 <- expandBracketLeft  j e bl
        br0 <- expandBracketRight j e br
        go (2 * e) bl0 br0

expandBracketBy
  :: Storable a
  => (a -> a -> a) -> Int -> a -> Vector a -> ST s (Vector a)
expandBracketBy f j e xs = do
  v  <- V.thaw xs
  xj <- V.unsafeRead v j
  V.unsafeWrite v j (f xj e)
  V.freeze v

expandBracketRight
  :: (Num a, Storable a)
  => Int -> a -> Vector a -> ST s (Vector a)
expandBracketRight = expandBracketBy (+)

expandBracketLeft
  :: (Num a, Storable a)
  => Int -> a -> Vector a -> ST s (Vector a)
expandBracketLeft  = expandBracketBy (-)

rejection
  :: (Ord b, Storable a, Fractional a)
  => (Vector a -> b)
  -> Int
  -> (a, a)
  -> b
  -> Vector a
  -> Prob (Vector a)
rejection f j bracket height = go where
  go zs = do
    u    <- uncurry uniform bracket
    let cool = runST $ do
          v <- V.thaw zs
          V.unsafeWrite v j u
          V.freeze v
    if   f cool < height
    then go cool
    else return cool


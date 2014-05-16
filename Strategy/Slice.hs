{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Strategy.Slice  where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.ST
import Data.Maybe
import Data.Vector.Storable (Vector, Storable)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as V hiding (length)
import Math.Probably.Sampler
import Math.Probably.Types

continuousSlice :: Maybe Double -> Transition Double
continuousSlice step = do
  Chain (_, cs) target _ _ <- get
  forM_ [0..V.length cs - 1] $ \j -> do
    Chain (ds, q) _ val t <- get
    let e       = fromMaybe t step
        curried = curry (logObjective target) ds
    height <- log <$> lift (uniform 0 $ exp val)

    let bracket = findBracket curried j e height q
    nq <- lift $ rejection curried j bracket height q

    put $ Chain (ds, nq) target (logObjective target (ds, nq)) e
  gets parameterSpacePosition

discreteSlice :: Maybe Int -> Transition Int
discreteSlice step = do
  Chain (ds, _) target _ _ <- get
  forM_ [0..V.length ds - 1] $ \j -> do
    Chain (q, cs) _ val t <- get
    let e       = fromMaybe t step
        curried = (flip (curry (logObjective target))) cs
    height <- log <$> lift (uniform 0 $ exp val)

    let bracket = findBracket curried j e height q
    nq <- lift $ rejectionDiscrete curried j bracket height q

    put $ Chain (nq, cs) target (logObjective target (nq, cs)) e
  gets parameterSpacePosition

findBracket
  :: (Ord a, Storable b, Num b)
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
  :: (Ord a, Storable t, Fractional t)
  => (Vector t -> a)
  -> Int
  -> (t, t)
  -> a
  -> Vector t
  -> Prob (Vector t)
rejection f j bracket height = go where
  go zs = do
    u  <- uncurry uniform bracket
    let cool = runST $ do
          v <- V.thaw zs
          V.unsafeWrite v j u
          V.freeze v
    if   f cool < height
    then go cool
    else return cool

-- 'uniform' is currently specialized to Fractional types, so polymorphism
-- doesn't fly without changing that.
rejectionDiscrete
  :: (Ord b, Storable a, Enum a)
  => (Vector a -> b)
  -> Int
  -> (a, a)
  -> b
  -> Vector a
  -> Prob (Vector a)
rejectionDiscrete f j bracket height = go where
  go zs = do
    u  <- oneOf [(fst bracket)..(snd bracket)]
    let cool = runST $ do
          v <- V.thaw zs
          V.unsafeWrite v j u
          V.freeze v
    if   f cool < height
    then go cool
    else return cool


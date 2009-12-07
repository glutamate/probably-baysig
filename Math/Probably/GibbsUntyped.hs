{-# LANGUAGE GADTs, TypeOperators, ScopedTypeVariables, Rank2Types #-}
module Math.Probably.GibbsUntyped where

import Math.Probably.Sampler
import Math.Probably.StochFun
import Numeric.LinearAlgebra hiding (flatten)
import Data.Array.ST
import Control.Monad.ST
import Control.Monad
import Data.STRef
import Data.Array
import GHC.Arr


type V = Vector Double

type VArr s = STArray s Int V 

type Gibbs s = [VArr s -> Sampler V]

myG :: Gibbs s
myG =  [\_-> return 0.5,
        \_->return 0.2]


{-gibbsSampler :: GTerm a b -> StochFun a b
gibbsSampler gibbs = SF stochfun
    where stochfun (x0, dbls) = undefined

-}

--there is a serious flaw in this approach. we need to replace items
--in x0 when running subsequent samplers. this is considerably more
--difficult.
runGibbs :: Gibbs s -> VArr s -> [Double] -> ST s [VArr s]
runGibbs gbs x0 dbls = do
  
--what a nightmare. What about the zipper??
  return []
  



{-nearlyGibbs ::  Gibbs -> StochFun VArr VArr
nearlyGibbs gbs = SF $ \(arr, dbls') -> runST $ do
                    dbls <- newSTRef dbls'
                    melted <- thawSTArray arr
                    forM_ (zip gbs [0..]) $ \(f,idx) -> do
                      rnds <- readSTRef dbls
                      let (x, rnds) = unSF $ f melted
                    ice <- freezeSTArray melted
                    newDbls <- readSTRef dbls
                    return (ice, newDbls) -}

{-
                                                                    (y, dbls') = sf dbls
                                                next = unSF $ nearlyGibbs g
                                                (rest, dbls'') = next (x0, dbls')
                                             in (y :+: rest, dbls'') -}
--nearlyGibbs GNil = sampler $ return TNil
 
{-

myChain = Mrkv (nearlyGibbs myG2) (2:+:2:+:TNil) id

tst = take 20 `fmap` runMarkovIO myChain


{-iterateG :: GTerm a b -> a -> b
iterateG (f :%: g) x0 = f x0 :+: iterateG g x0
iterateG GNil _ = TNil

myXs :: [Double :+: (Double :+: TNil)]
myXs = iterate (iterateG myG2) (2 :+: (3 :+: TNil))
-}

gibbs2 :: (a-> b) -> (b-> a) -> (a,b) -> (a,b)
gibbs2 f g (x,y) = let y1 = f x
                       x1 = g y1 in (x1,y1)

gibbs3 :: ((a, b)->c) -> ((b,c)-> a) -> ((c,a)-> b) -> (a,b,c) -> (a,b,c)
gibbs3 f g h (x0,y0,z0) = let x1 = g (y0,z0)
                              y1 = h (z0, x1)
                              z1 = f (x1, y1)
                          in (x1,y1,z1)


asD :: Double -> Double
asD = id
-}
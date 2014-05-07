{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Strategy.NUTS (nuts) where

import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Math.Probably.Sampler
import Math.Probably.Types
import Math.Probably.Utils

type Parameters = Vector Double
type Gradient   = Parameters -> Parameters
type Particle   = (Parameters, Parameters)

nuts :: Transition Double
nuts = do
    Chain t target _ store <- get
    r0          <- V.replicateM (V.length t) (lift $ normal 0 1)
    z0          <- lift $ expDist 1
    let logu     = log (auxilliaryTarget lTarget t r0) - z0
        lTarget  = logObjective target
        glTarget = handleGradient $ gradient target
        e        = getStepSize Nothing store

    let go (tn, tp, rn, rp, tm, j, n, s)
          | s == 1 = do
              vj <- lift $ oneOf [-1, 1]
              z  <- lift unit

              (tnn, rnn, tpp, rpp, t1, n1, s1) <- 
                if   vj == -1
                then do
                  (tnn', rnn', _, _, t1', n1', s1') <- 
                    buildTree lTarget glTarget tn rn logu vj j e
                  return (tnn', rnn', tp, rp, t1', n1', s1')
                else do
                  (_, _, tpp', rpp', t1', n1', s1') <- 
                    buildTree lTarget glTarget tp rp logu vj j e
                  return (tn, rn, tpp', rpp', t1', n1', s1')

              let accept = s1 == 1 && (min 1 (fi n1 / fi n :: Double)) > z

                  n2 = n + n1
                  s2 = s1 * stopCriterion tnn tpp rnn rpp
                  j1 = succ j
                  t2 | accept    = t1
                     | otherwise = tm

              go (tnn, tpp, rnn, rpp, t2, j1, n2, s2)

          | otherwise = do
              put $ Chain tm target (lTarget tm) (updateStepSize e store)
              return tm

    go (t, t, r0, r0, t, 0, 1, 1)

buildTree lTarget glTarget t r logu v 0 e = do
  let (t0, r0) = leapfrog glTarget (t, r) (v * e)
      jointL   = log $ auxilliaryTarget lTarget t0 r0
      n        = indicate (logu < jointL)
      s        = indicate (logu - 1000 < jointL)
  return (t0, r0, t0, r0, t0, n, s)

buildTree lTarget glTarget t r logu v j e = do
  z <- lift unit
  (tn, rn, tp, rp, t0, n0, s0) <- 
    buildTree lTarget glTarget t r logu v (pred j) e

  if   s0 == 1
  then do
    (tnn, rnn, tpp, rpp, t1, n1, s1) <- 
      if   v == -1
      then do
        (tnn', rnn', _, _, t1', n1', s1') <- 
          buildTree lTarget glTarget tn rn logu v (pred j) e
        return (tnn', rnn', tp, rp, t1', n1', s1')
      else do
        (_, _, tpp', rpp', t1', n1', s1') <- 
          buildTree lTarget glTarget tp rp logu v (pred j) e
        return (tn, rn, tpp', rpp', t1', n1', s1')

    let accept = (fi n1 / max (fi (n0 + n1)) 1) > (z :: Double)
        n2     = n0 + n1
        s2     = s0 * s1 * stopCriterion tnn tpp rnn rpp
        t2     | accept    = t1
               | otherwise = t0 

    return (tnn, rnn, tpp, rpp, t2, n2, s2)
  else return (tn, rn, tp, rp, t0, n0, s0)

stopCriterion :: Parameters -> Parameters -> Parameters -> Parameters -> Int
stopCriterion tn tp rn rp = 
      indicate (positionDifference `innerProduct` rn >= 0)
    * indicate (positionDifference `innerProduct` rp >= 0)
  where
    positionDifference = tp .- tn

leapfrog :: Gradient -> Particle -> Double -> Particle
leapfrog glTarget (t, r) e = (tf, rf) where 
  rm = adjustMomentum glTarget e t r
  tf = adjustPosition e rm t
  rf = adjustMomentum glTarget e tf rm

adjustMomentum :: (t -> Parameters) -> Double -> t -> Parameters -> Parameters
adjustMomentum glTarget e t r = r .+ ((e / 2) .* glTarget t)

adjustPosition :: Double -> Parameters -> Parameters -> Parameters
adjustPosition e r t = t .+ (e .* r)

auxilliaryTarget :: (t -> Double) -> t -> Parameters -> Double
auxilliaryTarget lTarget t r = exp (lTarget t - 0.5 * innerProduct r r)

innerProduct :: Parameters -> Parameters -> Double
innerProduct xs ys = V.sum $ V.zipWith (*) xs ys

indicate :: Integral a => Bool -> a
indicate True  = 1
indicate False = 0

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

getStepSize :: Maybe Double -> Tunables -> Double
getStepSize (Just e) _    = e
getStepSize Nothing store = e where
  (TDouble e) = lookupDefault (TDouble 0.1) NUTS store

updateStepSize :: Double -> Tunables -> Tunables
updateStepSize e = Map.insert NUTS (TDouble e) 


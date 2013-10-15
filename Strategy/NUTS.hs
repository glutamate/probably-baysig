{-# OPTIONS_GHC -Wall #-}

module Strategy.NUTS where

import Math.Probably.Sampler
import Math.Probably.MCMC
import Numeric.LinearAlgebra

type StateTree = 
  (Parameters, Parameters, Parameters, Parameters, Parameters, Int, Bool)

-- | The NUTS strategy.
nuts :: Strategy Double
nuts = GStrategy nutsTrans (const 0.1)

-- | NUTS transition kernel for step size e.
nutsTrans 
  :: PostGradF
  -> Parameters
  -> Double
  -> t
  -> Prob ((Parameters, Double), Maybe (PosteriorDensity, Gradient))
nutsTrans postGrad t e _ = do
  let lTarget  = fst . postGrad
      glTarget = snd . postGrad

  r0 <- fmap fromList $ normalManyUnit (dim t)
  z0 <- expDist 1  
  let logu = log (auxilliaryTarget lTarget t r0) - z0

  let go (tn, tp, rn, rp, tm, j, n, s)
        | s = do
            vj <- oneOf [-1, 1]
            z2 <- unit

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

            let accept = s1 && min 1 (fi n1 / fi n) > z2
                n2     = n + n1
                s2     = s1 && stopCriterion tnn tpp rnn rpp
                j1     = succ j
                t2     | accept    = t1
                       | otherwise = tm

            go (tnn, tpp, rnn, rpp, t2, j1, n2, s2)

        | otherwise = return ((tm, e), Nothing)

  go (t, t, r0, r0, t, 0, 1, True)

-- | Determine the candidate position / momentum states for a move.
buildTree 
  :: (Parameters -> Double)
  -> (Parameters -> Parameters)
  -> Parameters
  -> Parameters
  -> Double
  -> Int
  -> Int
  -> Double
  -> Prob StateTree
buildTree lTarget glTarget t r logu v 0 e = do
  let (t0, r0) = leapfrog glTarget (t, r) (fromIntegral v * e)
      lJoint   = log $ auxilliaryTarget lTarget t0 r0
      n        = indicate (logu < lJoint)
      stop     = logu - 1000 < lJoint
  return (t0, r0, t0, r0, t0, n, stop)

buildTree lTarget glTarget t r logu v j e = do
  z <- unit
  (tn, rn, tp, rp, t0, n0, stop0) <- 
    buildTree lTarget glTarget t r logu v (pred j) e

  if   stop0
  then do
    (tnn, rnn, tpp, rpp, t1, n1, stop1) <-
      if   v == -1
      then do 
        (tnn', rnn', _, _, t1', n1', s1') <- 
          buildTree lTarget glTarget tn rn logu v (pred j) e
        return (tnn', rnn', tp, rp, t1', n1', s1')
      else do
        (_, _, tpp', rpp', t1', n1', s1') <-
          buildTree lTarget glTarget tp rp logu v (pred j) e
        return (tn, rn, tpp', rpp', t1', n1', s1')
 
    let accept = (fi n1 / max (fi $ n0 + n1) 1) > z
        n2     = n0 + n1
        stop2  = stop0 && stop1 && stopCriterion tnn tpp rnn rpp
        t2     | accept    = t1
               | otherwise = t0 

    return (tnn, rnn, tpp, rpp, t2, n2, stop2)
  else return (tn, rn, tp, rp, t0, n0, stop0)
 
-- | When to stop doubling the candidate state tree.
stopCriterion :: Parameters -> Parameters -> Parameters -> Parameters -> Bool
stopCriterion tn tp rn rp = 
       positionDifference `dot` rn >= 0
    && positionDifference `dot` rp >= 0
  where
    positionDifference = tp `sub` tn

-- | The acceptance ratio for a proposed move.
acceptanceRatio
  :: PosteriorF
  -> Parameters
  -> Parameters
  -> Parameters
  -> Parameters
  -> Double
acceptanceRatio lTarget t0 t1 r0 r1 = auxilliaryTarget lTarget t1 r1
                                    / auxilliaryTarget lTarget t0 r0

-- | Joint density over position & momentum.
auxilliaryTarget :: PosteriorF -> Parameters -> Parameters -> Double
auxilliaryTarget lTarget t r = exp (lTarget t - 0.5 * r `dot` r)

-- | A single leapfrog step.
leapfrog 
  :: (Parameters -> Parameters)
  -> (Parameters, Parameters)
  -> Double
  -> (Parameters, Parameters)
leapfrog glTarget (t, r) e = (tf, rf)
  where 
    rm = adjustMomentum glTarget e t r
    tf = adjustPosition e rm t
    rf = adjustMomentum glTarget e tf rm

-- | Adjust a particle's momentum.
adjustMomentum 
  :: (Parameters -> Parameters)
  -> Double
  -> Parameters
  -> Parameters
  -> Parameters
adjustMomentum glTarget e t r = r `add` scale (e / 2) (glTarget t)

-- | Adjust a particle's position.
adjustPosition :: Double -> Parameters -> Parameters -> Parameters
adjustPosition e r t = t `add` scale e r

-- | Indicator function.
indicate :: Integral a => Bool -> a
indicate True  = 1
indicate False = 0

-- | Alias for fromIntegral.
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral


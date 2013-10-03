{-# OPTIONS_GHC -Wall #-}

module Strategy.NUTS where

import Math.Probably.Sampler
import Math.Probably.MCMC
import Numeric.LinearAlgebra

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
  let logu = auxilliaryTarget lTarget t r0 - z0

  let go (tn, tp, rn, rp, j, tm, n, s)
        | s = do
            z1 <- unit
            let vj | z1 > 0.5  = 1
                   | otherwise = -1
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

            let accept = (fi n1 / fi n) > z2

                t2 | accept    = t1
                   | otherwise = t

                n2 = n + n1
                s2 = s1 && ((tpp `sub` tnn) `dot` rnn >= 0)
                        && ((tpp `sub` tnn) `dot` rpp >= 0) 
                j1 = succ j

            go (tnn, rnn, tpp, rpp, j1, t2, n2, s2)

        | otherwise = return ((tm, e), Nothing)

  go (t, t, r0, r0, 0, t, 1, True)

-- | Build a binary tree representing admissible states for NUTS.
buildTree 
  :: (Parameters -> Double)
  -> (Parameters -> Parameters)
  -> Parameters
  -> Parameters
  -> Double
  -> Int
  -> Int
  -> Double
  -> Prob 
       (Parameters, Parameters, Parameters, Parameters, Parameters, Int, Bool)
buildTree lTarget glTarget t r logu v 0 e = do
  let (t0, r0)  = leapfrog glTarget (t, r) (fromIntegral v * e)
      auxTarget = auxilliaryTarget lTarget t0 r0
      n         = indicate (logu < auxTarget)
      stop      = logu - 1000 < auxTarget
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
 
    let p  = fromIntegral n1 / fromIntegral (n0 + n1)
        n2 = n0 + n1
        t2 | p > z     = t1
           | otherwise = t0 
        stop2 = stop1
             && ((tpp `sub` tnn) `dot` rnn >= 0)
             && ((tpp `sub` tnn) `dot` rpp >= 0)

    return (tnn, rnn, tpp, rpp, t2, n2, stop2)
  else return (tn, rn, tp, rp, t0, n0, stop0)
  
-- | The acceptance ratio for a proposed move.
acceptanceRatio
  :: (Parameters -> Double)
  -> Parameters
  -> Parameters
  -> Parameters
  -> Parameters
  -> Double
acceptanceRatio lTarget t0 t1 r0 r1 = auxilliaryTarget lTarget t1 r1
                                    / auxilliaryTarget lTarget t0 r0

-- | Joint density over position & momentum.
auxilliaryTarget :: (Parameters -> Double) -> Parameters -> Parameters -> Double
auxilliaryTarget lTarget t r = exp (lTarget t - 0.5 * r `dot` r)

-- | Alias for fromIntegral.
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

-- | The leapfrog integrator.
leapfrogIntegrator 
  :: Int 
  -> (Parameters -> Parameters)
  -> (Parameters, Parameters)
  -> Double
  -> (Parameters, Parameters)
leapfrogIntegrator n glTarget particle e = go particle n
  where 
    go state ndisc 
      | ndisc <= 0 = state
      | otherwise  = go (leapfrog glTarget state e) (pred n)

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


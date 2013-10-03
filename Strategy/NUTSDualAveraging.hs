module Strategy.NUTS where

import Math.Probably.Sampler
import Math.Probably.MCMC
import Numeric.LinearAlgebra

-- carry around e0, hbar, etc.. plus whether we're adapting or not
-- nutsDualAveraging nAdapt = GStrategy (nutsDualAveragingTrans nAdapt) undefined

-- | Build a binary tree representing admissible states for NUTS, with dual-
--   averaging.
buildTreeDualAveraging
  :: (Parameters -> Double)
  -> (Parameters -> Parameters)
  -> Parameters
  -> Parameters
  -> Double
  -> Int
  -> Int
  -> Double
  -> Parameters
  -> Parameters
  -> Prob 
       ( Parameters, Parameters, Parameters, Parameters, Parameters
       , Int, Bool, Double, Int)
buildTreeDualAveraging lTarget glTarget t r logu v 0 e t0 r0 = do
  let (t', r')  = leapfrog glTarget (t, r) (fromIntegral v * e)
      auxTarget = auxilliaryTarget lTarget t' r'
      n'        = indicate (logu < auxTarget)
      s'        = logu - 1000 < auxTarget
      a'        = min 1 (acceptanceRatio lTarget t0 t' r0 r')
      na'       = 1
  return $ (t', r', t', r', t', n', s', a', na')

buildTreeDualAveraging lTarget glTarget t r logu v j e t0 r0 = do
  z <- unit
  (tn, rn, tp, rp, t', n', s', a', na') <-
    buildTreeDualAveraging lTarget glTarget t r logu v (pred j) e t0 r0

  if   s'
  then do
    (tnn, rnn, tpp, rpp, t'', n'', s'', a'', na'') <-
      if   v == -1
      then do 
        (tnn', rnn', _, _, t1', n1', s1', a1', na1') <-
          buildTreeDualAveraging lTarget glTarget tn rn logu v (pred j) e t0 r0
        return $ (tnn', rnn', tp, rp, t1', n1', s1', a1', na1')
      else do
        (_, _, tpp', rpp', t1', n1', s1', a1', na1') <-
          buildTreeDualAveraging lTarget glTarget tp rp logu v (pred j) e t0 r0
        return $ (tn, rn, tpp', rpp', t1', n1', s1', a1', na1')
 
    let accept = (fromIntegral n'' / fromIntegral (n' + n'')) > z
        t2 | accept    = t''
           | otherwise = t'

        n2  = n' + n''
        s2  = and [ s''
                  , (tpp `sub` tnn) `dot` rnn >= 0
                  , (tpp `sub` tnn) `dot` rpp >= 0 ]

        a2  = a' + a''
        na2 = na' + na''

    return (tnn, rnn, tpp, rpp, t2, n2, s2, a2, na2)
  else return (tn, rn, tp, rp, t', n', s', a', na')

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
adjustMomentum glTarget e t r = r `add` (scale (e / 2) (glTarget t))

-- | Adjust a particle's position.
adjustPosition :: Double -> Parameters -> Parameters -> Parameters
adjustPosition e r t = t `add` (scale e r)

-- | Indicator function.
indicate :: Integral a => Bool -> a
indicate True  = 1
indicate False = 0

-- | Heuristic to calculate an initial step size.
findReasonableEpsilon
  :: (Parameters -> Double)
  -> (Parameters -> Parameters)
  -> Parameters
  -> Prob Double
findReasonableEpsilon lTarget glTarget t0 = do
  r0 <- fmap fromList $ normalManyUnit (dim t0)
  let (t1, r1) = leapfrog glTarget (t0, r0) 1.0
      a        = 2 * indicate (acceptanceRatio lTarget t0 t1 r0 r1 > 0.5) - 1

      go e t r | (acceptanceRatio lTarget t0 t r0 r) ^^ a > 2 ^^ (-a) = 
                   let (tn, rn) = leapfrog glTarget (t, r) e
                   in  go (2 ^^ a * e) tn rn 
               | otherwise = e

  return $ go 1.0 t1 r1


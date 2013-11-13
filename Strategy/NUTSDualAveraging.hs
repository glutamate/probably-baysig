module Strategy.NUTS where

import Math.Probably.Sampler
import Math.Probably.MCMC
import Numeric.LinearAlgebra

type StateTree = 
  ( Parameters, Parameters, Parameters, Parameters, Parameters
  , Int, Bool, Double, Int
  )

data DualAveragingParameters = DualAveragingParameters {
    mAdapt :: Int
  , delta  :: Double
  , mu     :: Double
  , gamma0 :: Double
  , tau0   :: Double
  , kappa  :: Double
  } deriving Show

data StepSizeParameters = StepSizeParameters {
    iteration    :: Int
  , stepSize     :: Double
  , meanStepSize :: Double
  , meanH        :: Double
  } deriving Show

-- | The NUTS strategy with dual averaging.
-- nutsDualAveraging :: Strategy Double
-- nutsDualAveraging = GStrategy nutsTrans (const 0.1)

-- | Transition kernel for NUTS with dual-averaging.
nutsTransDualAveraging
  :: PostGradF
  -> Parameters
  -> StepSizeParameters
  -> DualAveragingParameters
  -> Prob ((Parameters, StepSizeParameters), Maybe (PosteriorDensity, Gradient))
nutsTransDualAveraging postGrad t ssParams daParams = do
  let lTarget  = fst . postGrad
      glTarget = snd . postGrad
      m        = iteration ssParams
      e        = stepSize ssParams
      eAvg     = meanStepSize ssParams
      h        = meanH ssParams

  r0 <- fmap fromList $ normalManyUnit (dim t)
  z0 <- expDist 1  
  let logu = log (auxilliaryTarget lTarget t r0) - z0

  let go (tn, tp, rn, rp, tm, j, n, s, a, na)
        | s = do
            vj <- oneOf [-1, 1]
            z2 <- unit

            (tnn, rnn, tpp, rpp, t1, n1, s1, a1, na1) <-
              if   vj == -1
              then do
                (tnn', rnn', _, _, t1', n1', s1', a1', na1') <- 
                  buildTreeDualAveraging lTarget glTarget tn rn logu vj j e t r0
                return (tnn', rnn', tp, rp, t1', n1', s1', a1', na1')
              else do
                (_, _, tpp', rpp', t1', n1', s1', a1', na1') <- 
                  buildTreeDualAveraging lTarget glTarget tp rp logu vj j e t r0
                return (tn, rn, tpp', rpp', t1', n1', s1', a1', na1')

            let accept = s1 && (min (fi n1 / fi n) 1) > z2
                n2     = n + n1
                s2     = s1 && stopCriterion tnn tpp rnn rpp
                j1     = succ j
                t2     | accept    = t1
                       | otherwise = tm

            go (tnn, tpp, rnn, rpp, t2, j1, n2, s2, a1, na1)

        | otherwise = return (tm, a, na)

  (nextPosition, alpha, nalpha) <- go (t, t, r0, r0, t, 0, 1, True, 0, 0)

  let (hNext, eNext, eAvgNext) =
          if   m <= mAdapt daParams
          then (hm, exp logEm, exp logEbarM)
          else (h, eAvg, eAvg)
        where
          eta = 1 / (fromIntegral m + tau0 daParams)
          hm  = (1 - eta) * h 
              + eta * (delta daParams - alpha / fromIntegral nalpha)

          zeta = fromIntegral m ** (- (kappa daParams))

          logEm    = mu daParams - sqrt (fromIntegral m) / gamma0 daParams * hm
          logEbarM = (1 - zeta) * log eAvg + zeta * logEm

      nextStepSizeParams = StepSizeParameters {
          iteration    = succ m
        , stepSize     = eNext
        , meanStepSize = eAvgNext
        , meanH        = hNext
        }
 
  return ((t, nextStepSizeParams), Nothing)

-- | Build a binary tree representing admissible states for NUTS, with dual-
--   averaging.
buildTreeDualAveraging
  :: PosteriorF
  -> (Parameters -> Parameters)
  -> Parameters
  -> Parameters
  -> Double
  -> Int
  -> Int
  -> Double
  -> Parameters
  -> Parameters
  -> Prob StateTree 
buildTreeDualAveraging lTarget glTarget t r logu v 0 e t0 r0 = do
  let (t', r')   = leapfrog glTarget (t, r) (fromIntegral v * e)
      lJoint = log $ auxilliaryTarget lTarget t' r'
      n'     = indicate (logu < lJoint)
      s'     = logu - 1000 < lJoint
      a'     = min 1 (acceptanceRatio lTarget t0 t' r0 r')
      na'    = 1
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
 
    let p      = fi n'' / max (fi (n' + n'')) 1
        accept = p > z
        n2     = n' + n''
        s2     = s' && s'' && stopCriterion tnn tpp rnn rpp
        a2     = a' + a''
        na2    = na' + na''
        t2 | accept    = t''
           | otherwise = t'

    return (tnn, rnn, tpp, rpp, t2, n2, s2, a2, na2)
  else return (tn, rn, tp, rp, t', n', s', a', na')

-- | When to stop doubling the candidate state tree.
stopCriterion :: Parameters -> Parameters -> Parameters -> Parameters -> Bool
stopCriterion tn tp rn rp = 
       positionDifference `dot` rn >= 0
    && positionDifference `dot` rp >= 0
  where
    positionDifference = tp `sub` tn

-- | The acceptance ratio for a proposed move.
acceptanceRatio
  :: (Parameters -> Double)
  -> Parameters
  -> Parameters
  -> Parameters
  -> Parameters
  -> Double
acceptanceRatio lTarget t0 t1 r0 r1 = 
    auxilliaryTarget lTarget t1 r1
  / auxilliaryTarget lTarget t0 r0

-- | Joint density over position & momentum.
auxilliaryTarget :: (Parameters -> Double) -> Parameters -> Parameters -> Double
auxilliaryTarget lTarget t r = exp (lTarget t - 0.5 * r `dot` r)

-- | Alias for fromIntegral.
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

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
  :: PosteriorF
  -> (Parameters -> Parameters)
  -> Parameters
  -> Prob Double
findReasonableEpsilon lTarget glTarget t0 = do
  r0 <- fmap fromList $ normalManyUnit (dim t0)
  let (t1, r1) = leapfrog glTarget (t0, r0) 1.0
      a        = 2 * indicate (acceptanceRatio lTarget t0 t1 r0 r1 > 0.5) - 1

      go j e t r 
        | j <= 0 = e -- don't want to shrink initial estimate too much
        | (acceptanceRatio lTarget t0 t r0 r) ^^ a > 2 ^^ (-a) = 
            let (tn, rn) = leapfrog glTarget (t, r) e
            in  go (pred j) (2 ^^ a * e) tn rn 
        | otherwise = e

  return $ go 10 1.0 t1 r1

-- | Default DA parameters, given a base step size and burn in period.
basicDualAveragingParameters :: Double -> Int -> DualAveragingParameters
basicDualAveragingParameters step burnInPeriod = DualAveragingParameters {
    mu      = log (10 * step)
  , delta   = 0.5
  , mAdapt  = burnInPeriod
  , gamma0  = 0.05
  , tau0    = 10
  , kappa   = 0.75
  }


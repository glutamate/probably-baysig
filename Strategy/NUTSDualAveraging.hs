-- | See Hoffman, Gelman (2011) The No U-Turn Sampler: Adaptively Setting Path
--   Lengths in Hamiltonian Monte Carlo.

-- NOTE this needs to be tested a bit more stringently; lot of stuff changed
--      while porting it over.

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Strategy.NUTSDualAveraging (nutsDualAveraging) where

import Control.Monad.State.Strict
import qualified Data.Map as Map
import qualified Data.Vector.Storable as V
import Math.Probably.Sampler
import Math.Probably.Types
import Math.Probably.Utils

nutsDualAveraging :: Maybe Double -> Transition Double
nutsDualAveraging maybeStep = do
  c@(Chain t target _ store) <- get
  r0 <- V.replicateM (V.length t) (lift unormal)
  z0 <- lift $ expDist 1
  let logu     = log (auxilliaryTarget lTarget t r0) - z0
      lTarget  = logObjective target
      glTarget = handleGradient $ gradient target

  daParams <- lift $ getDaParams maybeStep c

  let e = daStep daParams

      go (tn, tp, rn, rp, tm, j, n, s, a, na)
        | s == 1 = do
            vj <- lift $ oneOf [-1, 1]
            z  <- lift unit

            (tnn, rnn, tpp, rpp, t1, n1, s1, a1, na1) <-
              if   vj == -1
              then do
                (tnn', rnn', _, _, t1', n1', s1', a1', na1') <- 
                  buildTreeDualAvg lTarget glTarget tn rn logu vj j e t r0
                return (tnn', rnn', tp, rp, t1', n1', s1', a1', na1')
              else do
                (_, _, tpp', rpp', t1', n1', s1', a1', na1') <- 
                  buildTreeDualAvg lTarget glTarget tp rp logu vj j e t r0
                return (tn, rn, tpp', rpp', t1', n1', s1', a1', na1')

            let accept = s1 == 1 && (min 1 (fi n1 / fi n :: Double)) > z 

                n2 = n + n1
                s2 = s1 * stopCriterion tnn tpp rnn rpp
                j1 = succ j
                t2 | accept    = t1
                   | otherwise = tm

            go (tnn, tpp, rnn, rpp, t2, j1, n2, s2, a1, na1)

        | otherwise = do 
            put $ Chain tm target (lTarget tm) store
            return (a, na)

  (alpha, nalpha) <- go (t, t, r0, r0, t, 0, 1, 1, 0, 0)
  
  let (hNext, eNext, eAvgNext) =
          if   mAdapt daParams <= 0
          then (hm, exp logEm, exp logEbarM)
          else (daH daParams, daStepAvg daParams, daStepAvg daParams)
        where
          eta = 1 / (fromIntegral (mAdapt daParams) + tau0 daParams)
          hm  = (1 - eta) * daH daParams 
              + eta * (delta daParams - alpha / fromIntegral nalpha)

          zeta = fromIntegral (mAdapt daParams) ** (- (kappa daParams))

          logEm    = mu daParams - sqrt (fromIntegral (mAdapt daParams)) / gammaP daParams * hm
          logEbarM = (1 - zeta) * log (daStepAvg daParams) + zeta * logEm

  let newDaParams = DualAveragingParameters {
          mAdapt = max 0 (pred $ mAdapt daParams)
        , delta  = delta daParams
        , mu     = mu daParams
        , gammaP = gammaP daParams
        , tau0   = tau0 daParams
        , kappa  = kappa daParams
        , daStep = eNext
        , daStepAvg = eAvgNext
        , daH       = hNext
        }

      newStore = updateDaParams newDaParams store

  modify (\s -> s { tunables = newStore })
  gets parameterSpacePosition

stopCriterion :: Parameters -> Parameters -> Parameters -> Parameters -> Int
stopCriterion tn tp rn rp = 
      indicate (positionDifference `innerProduct` rn >= 0)
    * indicate (positionDifference `innerProduct` rp >= 0)
  where
    positionDifference = tp .- tn

buildTreeDualAvg lTarget glTarget t r logu v 0 e t0 r0 = do
  let (t1, r1) = leapfrog glTarget (t, r) (v * e)
      jointL   = log $ auxilliaryTarget lTarget t1 r1
      n        = indicate (logu < jointL)
      s        = indicate (logu - 1000 <  jointL)
      a        = min 1 (acceptanceRatio lTarget t0 t1 r0 r1)
  return (t1, r1, t1, r1, t1, n, s, a, 1)
      
buildTreeDualAvg lTarget glTarget t r logu v j e t0 r0 = do
  z <- lift unit
  (tn, rn, tp, rp, t1, n1, s1, a1, na1) <- 
    buildTreeDualAvg lTarget glTarget t r logu v (pred j) e t0 r0

  if   s1 == 1
  then do
    (tnn, rnn, tpp, rpp, t2, n2, s2, a2, na2) <-
      if   v == -1
      then do 
        (tnn', rnn', _, _, t1', n1', s1', a1', na1') <- 
          buildTreeDualAvg lTarget glTarget tn rn logu v (pred j) e t0 r0
        return (tnn', rnn', tp, rp, t1', n1', s1', a1', na1')
      else do
        (_, _, tpp', rpp', t1', n1', s1', a1', na1') <-
          buildTreeDualAvg lTarget glTarget tp rp logu v (pred j) e t0 r0
        return (tn, rn, tpp', rpp', t1', n1', s1', a1', na1')

    let p      = fromIntegral n2 / max (fromIntegral (n1 + n2)) 1
        accept = p > (z :: Double)
        n3     = n1 + n2
        a3     = a1 + a2
        na3    = na1 + na2
        s3     = s1 * s2 * stopCriterion tnn tpp rnn rpp

        t3  | accept    = t2
            | otherwise = t1

    return (tnn, rnn, tpp, rpp, t3, n3, s3, a3, na3)
  else return (tn, rn, tp, rp, t1, n1, s1, a1, na1)

findReasonableEpsilon
  :: (Parameters -> Double) -> Gradient -> Parameters -> Prob Double
findReasonableEpsilon lTarget glTarget t0 = do
  r0 <- V.replicateM (V.length t0) unormal
  let (t1, r1) = leapfrog glTarget (t0, r0) 1.0
      a        = 2 * indicate (acceptanceRatio lTarget t0 t1 r0 r1 > 0.5) - 1
      go j e t r 
        | j <= 0 = e -- no need to shrink this excessively
        | (acceptanceRatio lTarget t0 t r0 r) ^^ a > 2 ^^ (-a) = 
            let (tn, rn) = leapfrog glTarget (t, r) e
            in  go (pred j) (2 ^^ a * e) tn rn 
        | otherwise = e

  return $ go 10 1.0 t1 r1

acceptanceRatio :: (t -> Double) -> t -> t -> Parameters -> Parameters -> Double
acceptanceRatio lTarget t0 t1 r0 r1 = auxilliaryTarget lTarget t1 r1
                                    / auxilliaryTarget lTarget t0 r0

updateDaParams :: DualAveragingParameters -> Tunables -> Tunables
updateDaParams daParams = Map.insert NUTSDualAveraging (TDAParams daParams)

getDaParams :: Maybe Double -> Chain Double -> Prob DualAveragingParameters
getDaParams Nothing (Chain t target _ store) = do
  let existing = Map.lookup NUTSDualAveraging store
      defaults = do
        step <- findReasonableEpsilon (logObjective target)
                  (handleGradient $ gradient target) t
        return $ defaultDualAveragingParameters step 100

  daParams <- case existing of
    Just (TDAParams daps) -> return daps
    Just _  -> defaults
    Nothing -> defaults

  return daParams

getDaParams (Just e) (Chain _ _ _ store) = do
  let existing = Map.lookup NUTSDualAveraging store
  daParams <- case existing of
    Just (TDAParams daps) -> return $ daps { daStep = e }
    Just _  -> return $ defaultDualAveragingParameters e 100
    Nothing -> return $ defaultDualAveragingParameters e 100
  
  return daParams


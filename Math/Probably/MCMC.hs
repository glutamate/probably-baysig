{-# LANGUAGE Arrows #-}

module Math.Probably.MCMC where

import Math.Probably.Sampler 
import qualified Math.Probably.PDF as P
import Math.Probably.StochFun
import Control.Arrow
import Data.List
import Data.Maybe
import Data.Ord
import Math.Probably.FoldingStats
import TNUtils
import Control.Monad
import Debug.Trace

--http://videolectures.net/mlss08au_freitas_asm/
rejection :: Double -> P.PDF a -> Sampler a -> P.PDF a -> Sampler a
rejection mult nicePDF niceSampler nastyPDF = rej  
    where rej = do  x <- niceSampler
                    u <- unitSample
                    if u < (nastyPDF x)/(mult * (nicePDF x))
                       then return x
                       else rej


importanceIO :: Fractional b => P.PDF a -> Sampler a -> (a->b) -> P.PDF a -> IO [b]
importanceIO nicePDF niceSampler f nastyPDF = do
  let markov = mvSampler (importance nicePDF niceSampler f nastyPDF) 
  means `fmap` runMarkovIO markov 

--dont read too much into the type
importance :: Fractional b => P.PDF a -> Sampler a -> (a->b) -> P.PDF a -> Sampler b
importance nicePDF niceSampler f nastyPDF = do
  x <- niceSampler
  return $ (f x)*(realToFrac $ (nastyPDF x)/(nicePDF x) )
                 
--andrieu intro mcmc for ml p 16 fig 5
metropolisHastings :: (a-> P.PDF a) -> (a->Sampler a) -> P.PDF a -> StochFun a a
metropolisHastings qPDF qSam p 
    = let accept xi xstar = min 1 $ (p xstar * qPDF xstar xi)/(p xi * qPDF xi xstar)
      in proc xi -> do
        u <- sampler unitSample -< ()
        xstar <- condSampler qSam -< xi
        returnA -< if u < accept xi xstar
                      then xstar
                      else xi


metropolis :: (a->Sampler a) -> P.PDF a -> StochFun a a
metropolis qSam p 
    = let accept xi xstar = min 1 $ (p xstar)/(p xi)
      in proc xi -> do
        u <- sampler unitSample -< ()
        xstar <- condSampler qSam -< xi
        returnA -< if u < accept xi xstar
                      then xstar
                      else xi

traceIt :: Show a => a -> a
traceIt x = trace (show x) x


metropolisLog ::(a->Sampler a) -> P.PDF a -> StochFun (a,Double) (a,Double)
metropolisLog qSam p 
    = let accept pi pstar =  min 1 $ exp (pstar - pi)
      in proc (xi, pi) -> do
        u <- sampler unitSample -< ()
        xstar <- condSampler qSam -< xi
        let pstar = p xstar
        returnA -< if u < accept pi pstar
                      then (xstar, pstar)
                      else (xi, pi)

samplingImportanceResampling :: Ord a => [(a,Double)] -> Sampler a
samplingImportanceResampling weightedSamples = 
  let sumWeights = sum $ map snd weightedSamples
      cummWeightedSamples = scanl (\(_,csum) (x,w) -> (x,csum+w)) (undefined,0) $ sortBy (comparing fst) weightedSamples
  in do
    u <- unitSample
    return . fst . fromJust $ find ((>=u*sumWeights) . snd) cummWeightedSamples
  
abcRej :: (th -> Sampler obs) -> (obs -> obs -> Bool) -> obs -> Sampler th -> Sampler th
abcRej  likelihood accept theData prior = abcrej
    where abcrej = do
            suggest <- prior
            sim <- likelihood suggest
            if accept sim theData
               then return suggest
               else abcrej

--parameter 
bayes :: Ord a => Int -> P.PDF a -> Sampler a -> IO (Sampler a)
bayes nsam likelihood prior = do
  let postsam = do
        theta <- prior
        let lh_theta = likelihood theta
        return (theta, lh_theta)
  weightedSamples <- take nsam `fmap` runSamplerIO postsam
  return $ samplingImportanceResampling weightedSamples

bayesMet :: (a->Sampler a) -> P.PDF a -> P.PDF a -> StochFun a a
bayesMet proposal lh prior = metropolis proposal (\x-> lh x * prior x)

bayesMetLog :: Show a => (a->Sampler a) -> [P.PDF a] -> a -> Markov a
bayesMetLog proposal pdfs inits = 
    let p x =  sum $ map ($x) pdfs
        p0 = p inits
    in Mrkv (metropolisLog proposal p) (inits, p0) (fst)

manyLike :: (theta -> a -> P.PDF b) -> ([(a,b)] -> P.PDF theta)
manyLike lh1 = \xys -> \theta -> product $ map (\(x,y) -> lh1 theta x y) xys

times :: Monad m => Int -> m a -> m [a]
times n ma = forM [1..n] $ const ma

test = 
  let xs = [1, 2, 3]
      ys = [2, 3.9, 6.1]
      lh (a, b, sd) x = P.gauss (a*x+b) sd
      prior = do a <- gauss (3) 0.5
                 b <- gauss (0) 0.5
                 sd <- uniform 0 5
                 return (a,b,sd)
  in do bsam <- bayes 10000 (manyLike lh $ zip xs ys) prior
        ps <- take 1000 `fmap` runSamplerIO bsam
        print $ meanSDF `runStat` (map fst3 ps)
        print $ meanSDF `runStat` (map snd3 ps)
        print $ regressF `runStat`  zip xs ys



--bayes :: P.PDF a -> P.PDF a -> StochFun a a
--bayes prior likelihood = let numerator = prior `mulPdf` likelihood


--sampler $ uniform 0 1

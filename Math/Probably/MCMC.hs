{-# LANGUAGE Arrows #-}

module Math.Probably.MCMC where

import Math.Probably.Sampler 
import qualified Math.Probably.PDF as P
import Math.Probably.StochFun
import Control.Arrow
import Data.List
import Data.Maybe
import Data.Ord

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

samplingImportanceResampling :: Ord a => [(a,Double)] -> Sampler a
samplingImportanceResampling weightedSamples = do
  let sumWeights = sum $ map snd weightedSamples
  let cummWeightedSamples = scanl (\(_,csum) (x,w) -> (x,csum+w)) (undefined,0) $ sortBy (comparing fst) weightedSamples
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


--bayes :: P.PDF a -> P.PDF a -> StochFun a a
--bayes prior likelihood = let numerator = prior `mulPdf` likelihood


--sampler $ uniform 0 1

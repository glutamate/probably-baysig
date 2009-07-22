module Math.Probably.MCMC where

import qualified Math.Probably.Sampler as S 
import qualified Math.Probably.PDF as P
import Math.Probably.StochFun

--http://videolectures.net/mlss08au_freitas_asm/
rejection :: Double -> P.PDF a -> S.Sampler a -> P.PDF a -> S.Sampler a
rejection mult nicePDF niceSampler nastyPDF = rej  
    where rej = do  x <- niceSampler
                    u <- S.unitSample
                    if u < (nastyPDF x)/(mult * (nicePDF x))
                       then return x
                       else rej


importanceIO :: Fractional b => P.PDF a -> S.Sampler a -> (a->b) -> P.PDF a -> IO [b]
importanceIO nicePDF niceSampler f nastyPDF = do
  let markov = mvSampler (importance nicePDF niceSampler f nastyPDF) 
  means `fmap` runMarkovIO markov 

--dont read too much into the type
importance :: Fractional b => P.PDF a -> S.Sampler a -> (a->b) -> P.PDF a -> S.Sampler b
importance nicePDF niceSampler f nastyPDF = do
  x <- niceSampler
  return $ (f x)*(realToFrac $ (nastyPDF x)/(nicePDF x) )
                                        

module Math.Probably.MCMC where

import qualified Math.Probably.Sampler as S 
import qualified Math.Probably.PDF as P

--http://videolectures.net/mlss08au_freitas_asm/
rejection :: Double -> P.PDF a -> S.Sampler a -> P.PDF a -> S.Sampler a
rejection mult nicePDF niceSampler nastyPDF = rej  
    where rej = do  x <- niceSampler
                    u <- S.unitSample
                    if u < (nastyPDF x)/(mult * (nicePDF x))
                       then return x
                       else rej


importanceIO :: Fractional b => P.PDF a -> S.Sampler a -> (a->b) -> P.PDF a -> Int ->  IO b
importanceIO nicePDF niceSampler f nastyPDF n = do
  fws <- take n `fmap` S.runSamplerIO (importance nicePDF niceSampler f nastyPDF) 
  return $ (sum fws)/(realToFrac n)


--dont read too much into the type
importance :: Fractional b => P.PDF a -> S.Sampler a -> (a->b) -> P.PDF a -> S.Sampler b
importance nicePDF niceSampler f nastyPDF = do
  x <- niceSampler
  return $ (f x)*(realToFrac $ (nastyPDF x)/(nicePDF x) )
                                        

--magic :: Fractional a => S.Sampler a -> S.Sampler a
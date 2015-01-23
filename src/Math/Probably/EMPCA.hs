{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}

module Math.Probably.EMPCA where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.HMatrix ((#>), app)
import Math.Probably.FoldingStats
import qualified Data.Vector.Storable as VS
import Foreign.Storable.Tuple ()
import Debug.Trace

import Math.Probably.PCA
import Math.Probably.Sampler
import Math.Probably.Types


data EmPcaBasis = EmPcaBasis {
   centering :: VS.Vector (Double,Double),
   components :: Mat
   } deriving Show

emPca :: Int -> Int -> [Vec] -> Prob EmPcaBasis
emPca k iters vecs = do
  let meansds = findCentre vecs

      n = length vecs
      p = VS.length $ head vecs

      dat = fromColumns $ map (centre meansds) vecs

      go c 0 = tr c
      go c iter =
        let x = inv (tr c <> c)<> tr c <> dat
            c1 = dat <> tr x <> inv (x<> tr x)
        in go c1 (iter-1)

  cinit <- fmap (p><k) $ sequence $ replicate (p*k) unit

  return $ EmPcaBasis meansds $ go cinit $ trdims "dat" dat $ trdims "cinit" cinit $ iters

applyEmPca :: EmPcaBasis -> Vec -> Vec
applyEmPca (EmPcaBasis cent com ) v =  com `app` centre cent v

findCentre :: [Vec] -> VS.Vector (Double,Double)
findCentre  = uncurry (VS.zipWith (,)) . runStat meanSDF

centre :: VS.Vector (Double,Double) -> Vec -> Vec
centre meansds v = VS.zipWith (\x (mn,sd) -> (x-mn)/sd) v meansds


trdims s m = trace (s++": "++show (rows m, cols m))

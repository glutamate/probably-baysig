{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}

module Math.Probably.EMPCA where

import Numeric.LinearAlgebra hiding (orth)
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.HMatrix ((#>), app, orth)
import Math.Probably.FoldingStats
import qualified Data.Vector.Storable as VS
import Foreign.Storable.Tuple ()
import Debug.Trace

import Data.Ord (comparing)
import Data.List (sortBy)

import Math.Probably.PCA
import Math.Probably.Sampler
import Math.Probably.Types


data EmPcaBasis = EmPcaBasis {
   centering :: VS.Vector (Double,Double),
   cfinal :: Mat,
   pcastat :: Mat
   } deriving Show

emPca :: Int -> Int -> Maybe (Matrix Double) -> [Vec] -> Prob EmPcaBasis
emPca k iters mcinit vecs = do
  let meansds = findCentre vecs

      n = length vecs
      p = VS.length $ head vecs

      dat = trdimit "dat" $ fromColumns $ map (centre meansds) vecs

      go c 0 = c
      go c iter =
        let x = inv (tr c <> c)<> tr c <> dat
            c1 = dat <> tr x <> inv (x<> tr x)
        in go c1 (iter-1)

  cinit <- case mcinit of
             Nothing -> fmap (p><k) $ sequence $ replicate (p*k) unit
             Just c -> return c
  let cfinal = go cinit iters
      cOrth :: Matrix Double
      cOrth =  trdispit "cOrth" $ orth cfinal
      xevec = trdispit "cevec" $ truePca $ tr cOrth <> dat
      evec = trdispit "evec" $ cOrth <>  xevec
--      truepca m = stat m

  --    (xevec, eval) = truepca (tr cOrth <> dat)
  return $ EmPcaBasis meansds cOrth evec

showC (m, iter) = "iter "++show iter++" C ="++dispf 2 m


applyEmPca :: EmPcaBasis -> Vec -> Vec
applyEmPca (EmPcaBasis cent _ vp) v =  tr vp <> centre cent v

findCentre :: [Vec] -> VS.Vector (Double,Double)
findCentre  = uncurry (VS.zipWith (,)) . runStat meanSDF

centre :: VS.Vector (Double,Double) -> Vec -> Vec
centre meansds v = VS.zipWith (\x (mn,_) -> x-mn) v meansds


trdims s m = trace (s++": "++show (rows m, cols m))

trdimit s m = trace (s++": "++show (rows m, cols m)) m

trdispit s m = trace (s++": "++dispf 3 m) m

truePca dataset_mn =
  let dataset = dataset_mn - asColumn (mean dataset_mn)
      cc = covN (tr dataset)
      (cdd,cvv) = eigSH cc
      ii = reverse $ map fst $ sortBy (comparing snd) $ zip [0..] $ VS.toList cdd
      evects = cvv ? ii
  in evects

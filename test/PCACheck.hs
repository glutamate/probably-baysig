module Main where

import Math.Probably.Types
import Math.Probably.EMPCA
import Math.Probably.PCA
import Math.Probably.Datasets
import Math.Probably.Sampler

import System.Environment
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import qualified Data.Vector.Storable as VS

main = do
  fnm:_ <- getArgs
  Right plants <- iris fnm
  let plantvs = map (VS.fromList . plantToVector) plants
      basis = stat $ fromRows $ plantvs
      (enc, _) = pcaN 4 basis
      covar = cov $ fromRows $ map enc plantvs
      covarPre = cov $ fromRows $ plantvs
  disp 2 $ covarPre
  disp 2 $ covar
  em <- sampleIO $ emPca 4 200 plantvs
  let covarEm = cov $ fromRows $ map (applyEmPca em) plantvs
  disp 2 $ covarEm

--  mapM_ print plants

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
      (enc, _) = pcaN 3 basis
      covar = cov $ fromRows $ map enc plantvs
      covarPre = cov $ fromRows $ plantvs
  disp 2 $ covarPre
  disp 2 $ covar
  em <- sampleIO $ emPca 3 10 (Just cinit) plantvs
  let covarEm = cov $ fromRows $ map (applyEmPca em) plantvs
  disp 2 $ covarEm

cinit = (4><3)
        [ 0.6794,    0.0841,    0.6963,
          0.0731,    0.5255,    0.2313,
          0.6621,    0.0939,    0.7125,
          0.5875,    0.4601,    0.0754  ]


--  mapM_ print plants

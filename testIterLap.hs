module Main where 

import Math.Probably.Sampler
import Math.Probably.RandIO
import Math.Probably.FoldingStats
import qualified Math.Probably.PDF as PDF

import Numeric.LinearAlgebra

import Math.Probably.IterLap

import Graphics.Gnewplot.Exec
import Graphics.Gnewplot.Types
import Graphics.Gnewplot.Style
import Graphics.Gnewplot.Panels
import Graphics.Gnewplot.Instances
import Graphics.Gnewplot.Histogram

--cov = (2><2) [0.1,0.08,0.08,0.1]

--mn = (2 |> [1,1])

cov3 = (3><3) [0.1,  0.02, 0.08,
               0.02, 0.1,  0.05,
               0.08, 0.05, 0.1]

mn3' = (3 |> [1,1,1])

mn3 = (3 |> [1.5,1.5,1.5])


main = runRIO $ do
  (mn', cov') <- sample $ improve 10 (exp . PDF.multiNormal mn3 cov3) (mn3', cov3)
  io $ print mn'
  io $ print cov'
--  io $ print ws
  
{-# LANGUAGE RankNTypes, BangPatterns, ScopedTypeVariables #-}

module Main where

import Math.Probably.MCMC
import qualified Math.Probably.PDF as PDF
import Math.Probably.RandIO
import Math.Probably.HamMC
import Math.Probably.FoldingStats
import Math.Probably.Sampler
import Control.Applicative
import Control.Monad

import Numeric.LinearAlgebra
import Numeric.AD
import Text.Printf

import Graphics.Gnewplot.Exec
import Graphics.Gnewplot.Types
import Graphics.Gnewplot.Style
import Graphics.Gnewplot.Panels
import Graphics.Gnewplot.Instances
import Graphics.Gnewplot.Histogram
import qualified Data.Vector.Storable as V

import Debug.Trace


import qualified Control.Monad.State.Strict as S


varf i = [10, 0.5, 1, 5, 0.1]!!(i `mod` 5)

corrf i j | i < j = corrf j i
          | otherwise = [-0.5, -0.2, 0, 0.2, 0.5]!!((i+j) `mod` 5)

covf (i,j) | i == j = varf i
           | otherwise = varf i * varf j * corrf i j

d = 3

muMVN = fromList $ replicate d 10
covMVN = buildMatrix d d covf 

(invSigmaMVN, (lndetMVN, _)) = invlndet covMVN

pdfMVN = PDF.multiNormalByInv lndetMVN invSigmaMVN muMVN

--pdfI :: [Double] -> Double
pdfI xs = sum $ map f $ zip xs (map varf [0..]) where
  f (x, var) = PDF.normal 10 var x

pdfI2 xs = sum $ map f $ zip xs (map varf [0..]) where
  f (x, var) = PDF.logNormal 0.1 var x

pdf3 [alpha, error_sd] = sum $ map f regrdata where
  f (age, height) = PDF.normalLogPdf (alpha) error_sd  height


pdf2 [alpha, error_sd, beta] = sum $ map f regrdata where
  f (age, height) = PDF.normalLogPdf (alpha+(age*beta)) error_sd  height

regrdata :: (Real a, Floating a) => [(a,a)]
regrdata = [ 35.1**6.3,
 1.5**	2.1,
 35.2**	5.1,
 4.5**	3.1,
 10.1**	3.9,
 80.4**	9.1,
 5.2**	3.2,
 60.1**	8.1,
 70.1**	9.1,
 40.6**	7] where (**) = (,)

-- split HMC paper p 5
nealCov = (2><2) [1, 0.95, 0.95, 1]
(nealCovInv, (nealLnDet,_)) = invlndet nealCov
nealMean = fromList [3,3]
nealPDF v = PDF.multiNormalByInv nealLnDet nealCovInv nealMean v
nealPostGrad v = 
  let p = nealPDF v
      tol = 0.001
      f (i,x) = let h = max (tol/10) $ tol * x
                    v' = v V.// [(i,x+h)]
                    v'' = v V.// [(i,x-h)]
                in (nealPDF v' - nealPDF v'')/(2*h)
      grad3 = fromList $ map f $ zip [0..] $ toList v
  in (p,grad3)


main = runRIO $ do
  let vinit = fromList [0,0]
  io $ print $ nealPostGrad vinit
  let pinit = fst $ nealPostGrad vinit
  let hmcp0 = HMCPar vinit 19 0 0.15 0 0 False    
  (hmcp1,vs) <- runHMC nealPostGrad 30 hmcp0                  
  io $ mapM print vs
  io $ print hmcp1
  return () 

covF i j = pure (-) <*> before meanF (\v -> v@>i * v@>j) 
                    <*> fmap (uncurry (*)) (both (before meanF (@>i)) (before meanF (@>j)))

--cache prev, use grad'?

{-# LANGUAGE RankNTypes, BangPatterns, ScopedTypeVariables #-}

module Main where

import Math.Probably.MCMC
import qualified Math.Probably.PDF as PDF
import Math.Probably.RandIO
import Math.Probably.HamMC
import Math.Probably.MALA
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
nealCov :: Matrix Double
neal_d = 500

nealCov = buildMatrix neal_d neal_d $ \(i,j) -> if i==j then realToFrac (i+1) / 100 else 0
--(2><2) [1, 0.95, 0.95, 2]
(nealCovInv, (nealLnDet,_)) = invlndet nealCov
nealMean = fromList $ replicate neal_d 3
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

covM1 :: Matrix Double
covM1 = (3><3) [1, 0,0, 
                0 , 2,0, 
                0, 0, 4]

means1, vars2 :: Vector Double
vars2 = fromList [1,2,4]

means1 = fromList [3,3,3]

(covMinv, (covMLnDet,_)) = invlndet covM1


main = runRIO $ do
  --io $ print $ nealCov
  --io $ print $ nealCovInv  
  vs1 <- sample $ sequence $ replicate 10000 $ multiNormal means1 covM1
  io $ print $ runStat meanSDF $ map (@>0) $ vs1
  io $ print $ runStat meanSDF $ map (@>1) $ vs1
  io $ print $ runStat meanSDF $ map (@>2) $ vs1
  vs1 <- sample $ sequence $ replicate 10000 $ mvnSampler means1 2 vars2
  io $ print $ runStat meanSDF $ map (@>0) $ vs1
  io $ print $ runStat meanSDF $ map (@>1) $ vs1
  io $ print $ runStat meanSDF $ map (@>2) $ vs1
  io $ print $ PDF.multiNormal means1 (scale 2 covM1) (fromList [2,2,2])
--  io $ print $ PDF.multiNormalByInv covMLnDet covMinv means1 (fromList [2,2,2])
--  io $ print $ PDF.multiNormalIndep vars2 means1  (fromList [2,2,2])
  io $ print $ mvnPDF means1 2 vars2  (fromList [2,2,2])
{-  let vinit = fromList $ replicate neal_d (-10)
  --io $ print $ nealPostGrad vinit
  let pinit = fst $ nealPostGrad vinit
--  let hmcp0 = HMCPar vinit 19 0 0.15 0 0 False    
  (vs) <- runMalaRioSimple (nealCov, nealCovInv, cholSH nealCov) nealPostGrad 2000 10 10 vinit                  
  --io $ forM (vs) $ \v -> print (v, nealPDF v)
  io $ print $ runStat meanSDF $ map (@>20) $ drop 10 $ reverse vs
  io $ print $ runStat meanSDF $ map (@>80) $ drop 10 $ reverse vs
  let have_ess = calcESSprim $ thin 10 vs
  io  $ putStrLn $ "ESS=" ++show have_ess -}
  --io $ print hmcp1
  return () 

thin n [] = []
thin n (x:xs) = x : thin n (drop n xs)


covF i j = pure (-) <*> before meanF (\v -> v@>i * v@>j) 
                    <*> fmap (uncurry (*)) (both (before meanF (@>i)) (before meanF (@>j)))

--cache prev, use grad'?

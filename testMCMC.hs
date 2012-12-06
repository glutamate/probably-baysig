{-# LANGUAGE RankNTypes, BangPatterns, ScopedTypeVariables #-}

module Main where

import Math.Probably.MCMC
import qualified Math.Probably.PDF as PDF
import Math.Probably.RandIO
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

main = runRIO $ do
  --make PDF, sample from it
  
  --io $ print covMVN
  let inisam = return $ fromList $ replicate d 1
      pdfIV = (pdf2 . toList) 
  --(p1,init2, cor) <- bestOfTwoCov inisam pdfIV
--  io $ print init2
--  io $ putStrLn $ "p1 = "++show p1
--  io $ print cor


  --let iniampar = mkAMPar init2 (scale 1 cor) p1 -}
  {-iniampar <- initAdaMetFromCov 1000 pdfIV init2 0
                                             (scale 0.2 $ cor) 
  vsamples <- runFixMetRioESS 6 (initFixMet iniampar) (pdfIV)   -}
  --vsamples <- runMala cor pdfI 10000 init2

  vers <- forM [0..0] $ \i -> do
    (p1,init2, cor) <- bestOfTwoCov inisam pdfIV
    io $ print init2
    io $ putStrLn $ "p1 = "++show p1
    --io $ print cor

{-    mcmcres <-  runMalaRioCodaESS cor pdf2 50 init2 
    let vsamples =mcmcres
        --pseries = map fst mcmcres 
    let (means, (vars, cov)) = runStat (both meanF $ both varF $ covF 0 1) vsamples
    io $ putStrLn $ "mala means = "++ (show $ toList means)

    let realvars = map varf [0..(d-1)]

--    let varError = runStat meanF $ map (**2) $ zipWith (\got real-> (got-real)/real) ( toList vars) (realvars)

    --io $ putStrLn $ "actual vars = "++ (show $ map varf [0..(d-1)])
    --io $ putStrLn $ "mala vars = "++ (show $ toList vars)

    --io $ putStrLn $ "var error = "++ (show $ varError)

    --io $ putStrLn $ "actual cov = 0" -- ++ show (covf (0,1))
    --io $ putStrLn $ "mala cov = "++ show cov
    io $ gnuplotOnScreen  $ (YFromZero $ HistoStyle "histeps" 50 $ map (@>0) vsamples)    

    --io $ gnuplotOnScreen  $ zip [(0::Double)..] pseries
    io $ gnuplotOnScreen  $ ("alpha", zip [(0::Double)..] $ map (@>0) vsamples)
    io $ gnuplotOnScreen  $ ("error_sd", zip [(0::Double)..] $ map (@>1) vsamples    )
    io $ gnuplotOnScreen  $ ("beta", zip [(0::Double)..] $ map (@>2) vsamples    )
    io $ gnuplotOnScreen  $ ("thedata", regrdata::[(Double,Double)])
-}    
 --   return varError
  
  --io $ putStrLn $ "\n\nmean var error = "++show (runStat meanSEMF vers)
--    io $ putStrLn $ "mala ess = "++ show (calcESSprim $ map (thinV 10) vsamples)

  --x <- sample $ mala1 cor pdfI (0.1, init2, 1)
  
                          
  --io $ print x
  return ()

covF i j = pure (-) <*> before meanF (\v -> v@>i * v@>j) 
                    <*> fmap (uncurry (*)) (both (before meanF (@>i)) (before meanF (@>j)))

--cache prev, use grad'?

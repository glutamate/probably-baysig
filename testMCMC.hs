{-# LANGUAGE RankNTypes, BangPatterns, ScopedTypeVariables #-}

module Main where

import Math.Probably.MCMC
import qualified Math.Probably.PDF as PDF
import Math.Probably.RandIO
import Math.Probably.MALA
import Math.Probably.FoldingStats
import Math.Probably.Sampler
import Control.Applicative

import Numeric.LinearAlgebra
import Numeric.AD
import Text.Printf


import qualified Control.Monad.State.Strict as S


varf i = [0.1, 0.5, 1, 5, 10]!!(i `mod` 5)

corrf i j | i < j = corrf j i
          | otherwise = [-0.5, -0.2, 0, 0.2, 0.5]!!((i+j) `mod` 5)

covf (i,j) | i == j = varf i
           | otherwise = varf i * varf j * corrf i j

d = 15

muMVN = fromList $ replicate d 10
covMVN = buildMatrix d d covf 

(invSigmaMVN, (lndetMVN, _)) = invlndet covMVN

pdfMVN = PDF.multiNormalByInv lndetMVN invSigmaMVN muMVN

--pdfI :: [Double] -> Double
pdfI xs = sum $ map f $ zip xs (map varf [0..]) where
  f (x, var) = PDF.normal 10 var x

main = runRIO $ do
  --make PDF, sample from it
  
  --io $ print covMVN
  let inisam = return $ fromList $ replicate d 1
      pdfIV = (pdfI . toList) 
  (p1,init2, cor) <- bestOfTwoCov inisam pdfIV
--  io $ print init2
  io $ putStrLn $ "p1 = "++show p1
--  io $ print cor


  {-let iniampar = mkAMPar init2 (scale 1 cor) p1 -}
  {-iniampar <- initAdaMetFromCov 1000 pdfIV init2 0
                                             (scale 0.2 $ cor) 
  vsamples <- runFixMetRioESS 50 (initFixMet iniampar) (pdfIV)  -}
  --vsamples <- runMala cor pdfI 10000 init2
  vsamples <- runMalaRioESS cor pdfI 100 init2 
  
  let (means, (vars, cov)) = runStat (both meanF $ both varF $ covF 0 1) vsamples
  io $ putStrLn $ "mala means = "++ (show $ toList means)

  let realvars = map varf [0..(d-1)]

  io $ putStrLn $ "actual vars = "++ (show $ map varf [0..(d-1)])
  io $ putStrLn $ "mala vars = "++ (show $ toList vars)

  io $ putStrLn $ "var error = "++ (show $ runStat meanF $ map (**2) $ zipWith (\got real-> (got-real)/real) ( toList vars) (realvars))

  io $ putStrLn $ "actual cov = 0" -- ++ show (covf (0,1))
  io $ putStrLn $ "mala cov = "++ show cov

--  io $ putStrLn $ "mala ess = "++ show (calcESSprim $ map (thinV 10) vsamples)

  --x <- sample $ mala1 cor pdfI (0.1, init2, 1)
  
  --io $ print x
  return ()

covF i j = pure (-) <*> before meanF (\v -> v@>i * v@>j) 
                    <*> fmap (uncurry (*)) (both (before meanF (@>i)) (before meanF (@>j)))

--cache prev, use grad'?

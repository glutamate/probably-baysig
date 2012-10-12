{-# LANGUAGE RankNTypes #-}

module Main where

import Math.Probably.MCMC
import qualified Math.Probably.PDF as PDF
import Math.Probably.RandIO
import Math.Probably.FoldingStats
import Math.Probably.Sampler
import Control.Applicative

import Numeric.LinearAlgebra
import Numeric.AD

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
  vsamples <- runFixMetRioESS 20 (initFixMet iniampar) (pdfIV) -}

  vsamples <- runMala cor pdfI 50000 init2 
  
  let (means, (vars, cov)) = runStat (both meanF $ both varF $ covF 0 1) vsamples
  io $ putStrLn $ "mala means = "++ (show $ toList means)

  let realvars = map varf [0..(d-1)]

  io $ putStrLn $ "actual vars = "++ (show $ map varf [0..(d-1)])
  io $ putStrLn $ "mala vars = "++ (show $ toList vars)

  io $ putStrLn $ "var error = "++ (show $ runStat meanF $ map (**2) $ zipWith (\got real-> (got-real)/real) ( toList vars) (realvars))

  io $ putStrLn $ "actual cov = 0" -- ++ show (covf (0,1))
  io $ putStrLn $ "mala cov = "++ show cov

 -- io $ putStrLn $ "mala ess = "++ show (calcESSprim $ map (thinV 10) vsamples)

  --x <- sample $ mala1 cor pdfI (0.1, init2, 1)
  
  --io $ print x
  return ()

covF i j = pure (-) <*> before meanF (\v -> v@>i * v@>j) 
                    <*> fmap (uncurry (*)) (both (before meanF (@>i)) (before meanF (@>j)))

--cache prev, use grad'?

mala1 :: Matrix Double -> (forall a. (Real a, Floating a) => [a]->a) 
         -> (Double, Vector Double, Double, Double) 
         -> Sampler (Double, Vector Double, Double, Double)
mala1 cov pdf (sigma, xi, tr, tracc) = do
  let pdfv :: Vector Double -> Double
      pdfv v = pdf $ map realToFrac $ toList $ v
  let gradient = fromList $ grad pdf $ toList xi
  let xstarMean = xi + scale (sigma/2) gradient
      xstarCov = scale sigma  cov
  xstar <- multiNormal xstarMean xstarCov
  u <- unitSample
  let ratio = pdfv xstar -- + PDF.multiNormal xstar xstarCov xi 
              - pdfv xi -- - PDF.multiNormal xi xstarCov xstar
  if u < exp ratio
     then return ((min 1.5 $ 1+kmala/tr)*sigma, xstar, tr+1, tracc+1)
     else return ((max 0.75 $ 1-kmala/tr)**1.2*sigma, xi, tr+1, tracc)

  
runMala :: Matrix Double -> (forall a. (Real a, Floating a) => [a]->a) 
         ->  Int -> Vector Double -> RIO [Vector Double]
runMala cov pdf nsam init = go nsam (1,init,1, 0) [] where
  go 0 (_,x,tr, tracc) xs = do io $ putStrLn $ "MALA accept = "++show (tracc/tr)
                               return xs
  go n y xs = do y1@(_,x,_,_) <- sample $ mala1 cov pdf y
                 go (n-1) y1 $ x:xs
  

kmala = 50

{-runMalaRioESS ::  Matrix Double -> (forall a. (Real a, Floating a) => [a]->a) 
                  ->  Int -> Vector Double -> RIO [L.Vector Double]
runMalaRioESS want_ess  pdf = do
    seed <- S.get
    (nseed, xs, amp) <- io $ go 200 seed ampar []
    S.put nseed
    return xs
     where go (0::Int) s amp vs = goChunks s amp vs
           go nn s amp vs = do let (!ampn, !ns) = unSam (fixedMet pdf amp) s
                               go (nn-1) ns (ampn) $ (ampPar ampn):vs
 
           goChunks s amp [] = do 
              (nseed, xs, namp) <- go 200 s amp []
              goChunks nseed namp xs
           goChunks s amp xs = do
              let have_ess = min (realToFrac $ count_accept amp) 
                             $ calcESS want_ess xs
                  drawn = length xs
              --putStrLn $ show $ ampPar amp
              putStrLn $ "ESS="++show have_ess++" from "++show drawn
                       ++" drawn accept ratio="++acceptS amp
              if have_ess > realToFrac want_ess
                 then return (s, xs, amp)
                 else do
                   let need_ess = min (realToFrac $ want_ess `div` 5) 
                                    $ max 1 
                                    $ realToFrac want_ess - have_ess
                       samples_per_es = realToFrac drawn/have_ess
                       to_do = max 50 $ min 2000 $ samples_per_es * need_ess * 1.2
                   putStrLn $ "now doing "++ show to_do
                   go (round to_do) s amp xs 
-}
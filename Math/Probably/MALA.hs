{-# LANGUAGE RankNTypes, BangPatterns, ScopedTypeVariables #-}

module Math.Probably.MALA where

import Math.Probably.MCMC
import qualified Math.Probably.PDF as PDF
import Math.Probably.RandIO
import Math.Probably.FoldingStats
import Math.Probably.Sampler
import Control.Applicative

import Numeric.LinearAlgebra
import Numeric.AD
import Text.Printf

import Statistics.Test.KolmogorovSmirnov
import Statistics.Test.MannWhitneyU
import qualified Data.Vector.Unboxed as U
import qualified Control.Monad.State.Strict as S



data MalaPar = MalaPar { mpXi :: Vector Double,
                         mpPi :: Double,
                         mpGradLast :: Maybe (Vector Double),
                         mpSigma :: Double,
                         mpCount :: Double,
                         mpAccept :: Double }

mala1 :: Matrix Double -> (forall a. (Real a, Floating a) => [a]->a) 
         -> MalaPar
         -> Sampler MalaPar
mala1 cov pdf (MalaPar xi pi mbgrad sigma tr tracc) = do
  let pdfv :: Vector Double -> Double
      pdfv v = pdf $ toList $ v
  let gradienti = case mbgrad of 
                   Just lastgrad -> lastgrad
                   Nothing -> fromList $ grad pdf $ toList xi
  let xstarMean = xi + scale (sigma/2) (cov <> gradienti)
      xstarCov = scale sigma  cov
  xstar <- multiNormal xstarMean xstarCov
  u <- unitSample
  let gradientStar = fromList $ grad pdf $ toList xstar
  let revJumpMean = xstar + scale (sigma/2) (cov <> gradientStar)
  let pstar = pdfv xstar
  let ratio = pstar  + PDF.multiNormal revJumpMean xstarCov xi
              - pi   - PDF.multiNormal xstarMean xstarCov xstar
  let tr' = max 1 tr
  if u < exp ratio
     then return $ MalaPar xstar pstar (Just gradientStar) 
                           ((min 1.4 $ 1+kmala/tr')*sigma) 
                           (tr+1) (tracc+1)
--                           sigma (tr+1) (tracc+1)
     else return $ MalaPar xi pi (Just gradienti) 
                           ((max 0.7143 $ 1-kmala/tr')**1.3*sigma) 
                           (tr+1) tracc
--                           sigma (tr+1) tracc

  
runMala :: Matrix Double -> (forall a. (Real a, Floating a) => [a]->a) 
         ->  Int -> Vector Double -> RIO [(Double,Vector Double)]
runMala cov pdf nsam init = go nsam mp1 [] where
  go 0 mpar xs = do io $ putStrLn $ "MALA accept = "++show (mpAccept mpar/mpCount mpar)
                    io $ putStrLn $ "MALA sigma = "++show (mpSigma mpar)
                    return xs
  go n y xs = do y1 <- sample $ mala1 cov pdf y
                 go (n-1) y1 $ (mpPi y1, mpXi y1):xs 
  pi = pdf $ toList $ init
  mp1 =  MalaPar init pi Nothing 1 0 0

runMalaMP :: Matrix Double -> (forall a. (Real a, Floating a) => [a]->a) 
         ->  Int -> MalaPar -> [(Double,Vector Double)] -> RIO (MalaPar, [(Double,Vector Double)])
runMalaMP cov pdf nsam init xs0 = go nsam init xs0 where
  go 0 mpar xs = do io $ putStrLn $ "MALA accept = "++show (mpAccept mpar/mpCount mpar)
                    io $ putStrLn $ "MALA sigma = "++show (mpSigma mpar)
                    return (mpar, xs)
  go n y xs = do y1 <- sample $ mala1 cov pdf y
                 go (n-1) y1 $ (mpPi y1, mpXi y1):xs 
  pi = pdf $ toList $ mpXi init

  
 
kmala = 10

runMalaRioESS ::  Matrix Double -> (forall a. (Real a, Floating a) => [a]->a) 
                  ->  Int -> Vector Double -> RIO [Vector Double]
runMalaRioESS cov pdf want_ess xi = do
    let p0 = pdf $ toList $ xi
    let sigma0 = 1.5 / (realToFrac $ dim xi) -- determined empirically
    let mp0 = MalaPar xi p0 Nothing sigma0 0 0 
        nsam0 = want_ess*20
    io $ putStrLn $ "initial sigma = "++show (mpSigma mp0)
    (mp1, xs1) <- runMalaMP cov pdf nsam0 mp0 []
    let have_ess = min (mpAccept mp1) $ calcESSprim $ map snd xs1
    if have_ess > realToFrac want_ess
       then return $ map snd xs1
       else do let need_ess =  max 1 $ realToFrac want_ess - have_ess
                   samples_per_es = realToFrac nsam0/have_ess
                   to_do = round $ samples_per_es * need_ess 
               (mp2, xs2) <- runMalaMP cov pdf to_do mp1 xs1
               return  $ map snd  xs2 

runMalaRioCodaESS ::  Matrix Double -> (forall a. (Real a, Floating a) => [a]->a) 
                  ->  Int -> Vector Double -> RIO [Vector Double]
runMalaRioCodaESS cov pdf want_ess xi = do
    let p0 = pdf $ toList $ xi
    let sigma0 = 1.5 / (realToFrac $ dim xi) -- determined empirically
    let mp0 = MalaPar xi p0 Nothing sigma0 0 0 
        nsam0 = want_ess*5
    io $ putStrLn $ "initial sigma = "++show (mpSigma mp0)
    (mp1, xs1) <- runMalaMP cov pdf nsam0 mp0 []
    let converged mp xs = do
         let have_ess = min (mpAccept mp1) $ calcESSprim $ map snd xs1
         if have_ess > realToFrac want_ess
            then return $ map snd xs1
            else do let need_ess =  max 1 $ realToFrac want_ess - have_ess
                        samples_per_es = realToFrac nsam0/have_ess
                        to_do = round $ samples_per_es * need_ess 
                    (mp2, xs2) <- runMalaMP cov pdf to_do mp1 xs1
                    return $ map snd xs2 
    let go mp n xs = do
            (mp2, xs2) <- runMalaMP cov pdf n mp []
            if  mannWhitneyUtest TwoTailed 0.05 (U.fromList $ map fst xs2)
                                            (U.fromList $ map fst xs) /= Just NotSignificant
               then go mp2 (round $ realToFrac n*1.5) xs2
               else converged mp2 (xs2++xs)

    go mp1 nsam0 xs1



acceptSM ampar  | mpCount ampar == 0 = "0/0"
               | otherwise = printf "%.3g" (rate::Double) ++ " ("++show yes++"/"++show total++")" where
   rate = realToFrac (yes) / realToFrac (total)
   yes = mpAccept ampar
   total = mpCount ampar
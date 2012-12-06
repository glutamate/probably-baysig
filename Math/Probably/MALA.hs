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
import System.IO
import Data.Maybe

import Statistics.Test.KolmogorovSmirnov
import Statistics.Test.MannWhitneyU
import qualified Data.Vector.Unboxed as U
import qualified Control.Monad.State.Strict as S

import Debug.Trace
import Data.IORef

data MalaPar = MalaPar { mpXi :: !(Vector Double),
                         mpPi :: !Double,
                         mpGradLast :: !(Vector Double),
                         mpSigma :: !Double,
                         mpCount :: !Double,
                         mpAccept :: !Double,
                         mpFreezeSigma :: !Bool }
  deriving Show

data Pair a b = Pair !a !b

data List a = Cons !a !(List a)
            | Nil

unPair (Pair x y) = (x,y)
unList (Cons x xs) = x: unList xs
unList Nil =[]

mala1 :: Matrix Double -> (Vector Double -> (Double,Vector Double)) 
         -> MalaPar
         -> Sampler MalaPar
mala1 cov postgrad (MalaPar xi pi gradienti sigma tr tracc freeze) = do
                
  let xstarMean = xi + scale (sigma/2) (cov <> gradienti)
      xstarCov = scale sigma  cov
  xstar <- multiNormal xstarMean xstarCov
  u <- unitSample
  let (!pstar, !gradientStar) = postgrad xstar
  let !revJumpMean = xstar + scale (sigma/2) (cov <> gradientStar)
  
  let ratio = pstar  + PDF.multiNormal revJumpMean xstarCov xi
              - pi   - PDF.multiNormal xstarMean xstarCov xstar
  let tr' = max 1 tr
  if u < exp ratio
     then return $ MalaPar xstar pstar (gradientStar) 
                           (if freeze then sigma else (min 1.4 $ 1+kmala/tr')*sigma) 
                           (tr+1) (tracc+1) freeze
--                           sigma (tr+1) (tracc+1)
     else return $ MalaPar xi pi ( gradienti) 
                           (if freeze then sigma else (max 0.7143 $ 1-kmala/tr')**1.3*sigma) 
                           (tr+1) tracc freeze
--                           sigma (tr+1) tracc

  
runMala :: Matrix Double -> (Vector Double -> (Double,Vector Double)) 
         ->  Int -> Vector Double -> RIO [(Double,Vector Double)]
runMala cov postgrad nsam init = go nsam mp1 [] where
  go 0 mpar xs = do io $ putStrLn $ "MALA accept = "++show (mpAccept mpar/mpCount mpar)
                    io $ putStrLn $ "MALA sigma = "++show (mpSigma mpar)
                    return xs
  go n y xs = do y1 <- sample $ mala1 cov postgrad y
                 go (n-1) y1 $ (mpPi y1, mpXi y1):xs 
  (pi, gradi) = postgrad init
  mp1 =  MalaPar init pi (gradi)  1 0 0 False

runMalaMP :: Matrix Double -> (Vector Double -> (Double,Vector Double)) 
         ->  Int -> MalaPar -> [(Double,Vector Double)] -> RIO (MalaPar, [(Double,Vector Double)])
runMalaMP cov pdf nsam init xs0 = go nsam init xs0 where
  go 0 mpar xs = do io $ putStrLn $ "MALA accept = "++show (mpAccept mpar/mpCount mpar)
                    io $ putStrLn $ "MALA sigma = "++show (mpSigma mpar)
                    return (mpar, xs)
  go n y xs = do y1 <- sample $ mala1 cov pdf y
                 io $ putStr "." >> hFlush stdout
                 go (n-1) y1 $ (mpPi y1, mpXi y1):xs 


runMalaMPaccept' :: Matrix Double -> (Vector Double -> (Double,Vector Double)) 
         ->  Int -> MalaPar -> RIO (MalaPar, [(Double,Vector Double)])
runMalaMPaccept'  cov pdf nsam init  = do
  stseed <- S.get
  res <- io $ do
   seedref <- newIORef stseed
   resRef <- newIORef Nil
   let go !mpar  
        | (round $ mpAccept mpar) >= nsam 
          = return mpar
        | otherwise 
          = do seed <- readIORef seedref
               putStr ((show (round $ mpAccept mpar)) ++".") >> hFlush stdout
               let (!mpar1, !seed1) = unSam (mala1 cov pdf mpar) seed
               writeIORef seedref seed1
               modifyIORef resRef (Cons (Pair (mpPi mpar1) ( mpXi mpar1)))
               go mpar1
   mpar <- go init
   seed <- readIORef seedref
   reslist <- readIORef resRef
   return (seed, (mpar,reslist))
  S.put $ fst res
  let mp = fst $ snd res
  return $ (mp, map unPair $ unList $ snd $ snd res)
 
--  pi = pdf $ toList $ mpXi init

runMalaMPaccept :: Matrix Double -> (Vector Double -> (Double,Vector Double)) 
         -> Int -> MalaPar ->  RIO (MalaPar, [(Double,Vector Double)])
runMalaMPaccept cov pdf nsam init = go init [] where
  go !mpar !xs
    | (round $ mpAccept mpar) >= nsam = do
         io $ putStrLn $ "MALA accept = "++show (mpAccept mpar/mpCount mpar)
         io $ putStrLn $ "MALA sigma = "++show (mpSigma mpar)
         return (mpar, xs)
    | otherwise = do
         !mpar1 <- sample $ mala1 cov pdf mpar
         io $ putStr ((show (round $ mpAccept mpar)) ++".") >> hFlush stdout
         go mpar1 $ (mpPi mpar1, mpXi mpar1):xs
 
kmala = 5

runMalaRioESS ::  Matrix Double -> (Vector Double -> (Double,Vector Double)) 
                  ->  Int -> Vector Double -> RIO [Vector Double]
runMalaRioESS cov pdf want_ess xi = do
    let (p0,grad0) = pdf xi
    let sigma0 = 1.5 / (realToFrac $ dim xi) -- determined empirically
    let mp0 = MalaPar xi p0 grad0 sigma0 0 0 False
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

runMalaRioCodaESS ::  Matrix Double -> (Vector Double -> (Double,Vector Double)) 
                  ->  Int -> Vector Double -> RIO [Vector Double]
runMalaRioCodaESS cov pdf want_ess xi = do
    let (p0,grad0) = pdf xi
    let sigma0 = 1.5 / (realToFrac $ dim xi) -- determined empirically
    let mp0 = MalaPar xi p0 grad0 sigma0 0 0 False
        nsam0 = want_ess*1
    io $ putStrLn $ "initial sigma = "++show (mpSigma mp0)
    (mp1, xs1) <- runMalaMPaccept cov pdf nsam0 mp0 
    io $ print mp1
    let converged mp xs = do
         let have_ess = min (mpAccept mp1) $ calcESSprim $ map snd xs1
         if have_ess > realToFrac want_ess
            then return $ map snd xs1
            else do let need_ess =  max 1 $ realToFrac want_ess - have_ess
                        samples_per_es = realToFrac nsam0/have_ess
                        to_do = round $ samples_per_es * need_ess 
                    io $ putStrLn $ "running converged for "++show to_do
                    (mp2, xs2) <- runMalaMP cov pdf to_do (mp {mpFreezeSigma = True}) xs1
                    io $ putStrLn $ "All done with: "
                    io $ print mp2
                    return $ map snd xs2 
    let go mp n xs = do
            (mp2, xs2) <- runMalaMP cov pdf n mp []
            if  mannWhitneyUtest TwoTailed 0.10 (U.fromList $ map fst xs2)
                                            (U.fromList $ map fst xs) /= Just NotSignificant
               then do io$ putStrLn "not converged"
                       go mp2 (round $ realToFrac n*1.2) xs2
               else do io$ putStrLn "converged!"
                       io $ print mp2
                       converged mp2 (xs2++xs)

    go mp1 (round $ mpCount mp1) xs1



acceptSM ampar  | mpCount ampar == 0 = "0/0"
               | otherwise = printf "%.3g" (rate::Double) ++ " ("++show yes++"/"++show total++")" where
   rate = realToFrac (yes) / realToFrac (total)
   yes = mpAccept ampar
   total = mpCount ampar
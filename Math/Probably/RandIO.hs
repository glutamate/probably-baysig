{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, NoMonomorphismRestriction, BangPatterns, FlexibleInstances, ViewPatterns #-}
module Math.Probably.RandIO where

import qualified Math.Probably.PDF as PDF
import Math.Probably.Sampler
import Math.Probably.MCMC hiding (traceIt)
import Math.Probably.StochFun
import Math.Probably.FoldingStats
import Math.Probably.Sampler
import qualified Control.Monad.State.Strict as S
import System.IO
import qualified Data.Packed.Matrix as L
import Control.Monad.Trans
import Control.Monad
import Data.IORef
import qualified Numeric.LinearAlgebra as L
import Math.Probably.NelderMead

import Debug.Trace

type RIO = S.StateT Seed IO

runRIO :: RIO a -> IO a
runRIO mx = do
  sd <- getSeedIO
  S.evalStateT mx sd
  
io :: IO a -> RIO a
io = liftIO

sample :: Sampler a -> RIO a
sample (Sam f) = do sd <- S.get
                    let (x,sd') = f sd
                    S.put sd'
                    return x

update :: IORef a -> (a->Sampler a) -> RIO ()
update rf sm = do
  x <- io $ readIORef rf
  newx <- sample $ sm x
  io $ writeIORef rf newx

runChainRIO :: Int -> (a->String) -> a -> (a->Sampler a) -> RIO [a]
runChainRIO n showit init sam = do
    seed <- S.get
    (nseed, xs) <- io $ go n seed init []
    S.put nseed
    return xs
     where go 0 s x xs = return (s, xs)
           go nn s x xs = do let (!xx, !ns) = unSam (sam x) s
                             when(nn `rem` chsz==0) $ 
                                 putStrLn $ show (((n-nn) `div` chsz)*2)++"%: " ++showit xx
                             go (nn-1) ns xx $ xx:xs
           chsz = n `div` 50

data AdaMetRunPars = AdaMetRunPars 
     { nmTol :: Double,
       displayIt :: Maybe (L.Vector Double -> String),
       amnsam :: Int }

defaultAM = AdaMetRunPars 0.5 Nothing 1000

traceit s x = trace (s++show x) x

laplaceApprox :: AdaMetRunPars -> PDF.PDF (L.Vector Double) -> [Int] -> [((Int, Int), Double)] 
              -> L.Vector Double -> (L.Vector Double, Maybe (L.Matrix Double), Simplex)
laplaceApprox (AdaMetRunPars nmtol dispit nsam) pdf isInt fixed init =
     let iniSim = genInitial (negate . pdf) isInt 0.1 $ init
         finalSim =  traceit "finalsim" $ goNm (negate . pdf) isInt nmtol iniSim 

         (maxPost,hess) = hessianFromSimplex (negate . pdf) isInt fixed finalSim 
--     io $ print maxPost
         mbcor = case L.mbCholSH $ hess of 
                   Just _ -> Just $ L.inv hess
                   Nothing -> case L.mbCholSH $ posdefify hess of
                                 Just _ -> Just $ L.inv $ posdefify hess
                                 Nothing -> Nothing
         initV = centroid finalSim
     in (initV, mbcor, finalSim)

nmAdaMet :: AdaMetRunPars -> PDF.PDF (L.Vector Double) -> [Int] -> [((Int, Int), Double)] 
            -> L.Vector Double -> RIO [L.Vector Double]
nmAdaMet (AdaMetRunPars nmtol dispit nsam) pdf isInt fixed init = do
     let iniSim = genInitial (negate . pdf) isInt 0.1 $ init
     io $ print iniSim
     let finalSim =  goNm (negate . pdf) isInt nmtol iniSim
     io $ print finalSim
     let (maxPost,hess) = hessianFromSimplex (negate . pdf) isInt fixed finalSim 
--     io $ print maxPost
     io $ putStrLn "hessian"
     io $ print hess

     let mbcor = case L.mbCholSH hess of 
                   Just _ -> Just $ L.inv hess
                   Nothing -> Nothing  
     io $ putStrLn "maybe inverse hess"                                          
     io $ print mbcor
     let initV = centroid finalSim
     let mbcorChol = mbcor >>= L.mbCholSH 
     case (mbcor, mbcorChol) of
       (Just cor, Just _) ->  do --io $ putStrLn "chol cor"
                                 --io $ print $ L.inv cor
                                 let ampar = AMPar initV initV cor 2.4 (pdf initV) 0 0
                                 runAdaMetRIO nsam True ampar pdf
       _         -> do iniampar <- sample $ initialAdaMet 100 5e-3 pdf initV
                       froampar <- runAndDiscard (nsam*2) (show . ampPar) iniampar $ adaMet False pdf
                       runAdaMetRIO (nsam*2) True froampar pdf
      
                                      

runAdaMetRIO :: Int -> Bool -> AMPar -> PDF.PDF (L.Vector Double) -> RIO [L.Vector Double]
runAdaMetRIO n freeze ampar pdf = do
    seed <- S.get
    (nseed, xs) <- io $ go n seed ampar []
    S.put nseed
    return xs
     where go 0 s amp vs = do print $ amp
                              return (s, reverse vs)
           go nn s amp vs = do let (!ampn, !ns) = unSam (adaMet freeze pdf amp) s
                               when(nn `rem` chsz==0) $ 
                                   putStrLn $ show (((n-nn) `div` chsz)*2)++"%: " ++show (ampPar ampn)-- ++" LH="++show (pdf (ampPar ampn))
                               go (nn-1) ns ampn $ (ampPar ampn):vs
           chsz = n `div` 50

runAdaMetRIOInterleaveInitial :: Int -> Bool -> L.Matrix Double -> AMPar -> PDF.PDF (L.Vector Double) -> RIO [L.Vector Double]
runAdaMetRIOInterleaveInitial n freeze cov ampar pdf = do
    seed <- S.get
    (nseed, xs) <- io $ go n seed ampar []
    S.put nseed
    return xs
     where go 0 s amp vs = do print $ amp
                              return (s, vs)
           go nn s amp vs = do let (!ampn, !ns) = unSam (adaMetInterleaveInitial freeze cov pdf amp) s
                               when(nn `rem` chsz==0) $ 
                                   putStrLn $ show (((n-nn) `div` chsz)*2)++"%: " ++show (ampPar ampn)-- ++" LH="++show (pdf (ampPar ampn))
                               go (nn-1) ns ampn $ (ampPar ampn):vs
           chsz = n `div` 50
    

runChainStat :: Int -> (Fold a b) -> a -> (a->Sampler a) -> RIO b
runChainStat n (F acc fldini k _) ini sam = do
    seed <- S.get
    let (nseed, stat) = go n seed fldini ini 
    S.put nseed
    return stat
     where go 0 s y x = (s, k y)
           go nn s y x = let (xx, ns) = unSam (sam x) s
                         in go (nn-1) ns (acc y xx) xx


runAndWriteTo :: String -> Int -> (a -> String) -> a -> (a->Sampler a) -> RIO a
runAndWriteTo fnm n showit x sam = do
    h <- io $ openFile fnm WriteMode
    seed <- S.get
    (y,nseed) <- io $ runChainPrim h n showit seed x sam
    io $ hClose h
    S.put nseed
    return y 

runChainPrim :: Handle -> Int -> (a -> String) -> Seed -> a -> (a->Sampler a) -> IO (a, Seed)
runChainPrim h n showit seed x sam = go n seed x
   where go 0 s x = return (x,s)
         go nn s x = do
            let (y, ns) = unSam (sam x) s
            hPutStrLn h $ showit y
            when(nn `rem` chsz==0) $ 
              putStrLn $ show (((n-nn) `div` chsz)*2)++"%: "++showit y
            go (nn-1) ns y 
         chsz = n `div` 50

runOnce :: a -> (a->Sampler a) -> RIO a
runOnce init sam = runAndDiscardPrim 1 init sam


runAndDiscard :: Int -> (a->String) -> a -> (a->Sampler a) -> RIO a
--runAndDiscard = runAndDiscardPrim
runAndDiscard 0 _ x _ = return x
runAndDiscard n showit x sam = do
    go 50 x
 where chunksz = n `div` 50
       go 0 x = io (putStr "\n") >> return x
       go n x = do
          x' <- runAndDiscardPrim chunksz x sam
          io $ putStrLn $ show ((50-n)*2+2)++"%: "++showit x'
          go (n-1) x' 

runMeanVar :: Floating b => Int -> (a->b) -> a -> (a->Sampler a) -> RIO (b,b)
runMeanVar n conv init sam = do
   xs <- sample $ runChainS n init sam
   return $ both meanF stdDevF `runStat` map conv xs
   


runAndDiscardPrim :: Int -> a -> (a->Sampler a) -> RIO a
runAndDiscardPrim n init sam = do
   seed <- S.get
   let (x,nseed) = runPrim seed n init sam
   S.put nseed
   return x

runPrim :: Seed -> Int -> a -> (a->Sampler a) -> (a,Seed)
runPrim s 0 x _ = (x,s)
runPrim s n x sam = let (x', s') = unSam (sam x) s
                    in runPrim s' (n-1) x' sam

metSample2P :: Show a => String -> (Double -> a -> a -> Sampler a) 
               -> PDF.PDF (a,b) -> (Param a,b) -> Sampler (Param a)
metSample2P st prop p (par@(Param j t tt _ curw ada ini xi),y) 
    = let accept xi pi pstar | notNanInf2 pi pstar =  min 1 $ exp (pstar - pi)
                             | otherwise = cond [(nanOrInf pi && nanOrInf pstar, 
                                                        error $ "metropolisLnP "++st++" pi pi pstar :"++
                                                                show (pi,pstar)++"\n"++
                                                                show xi),
                                                 (nanOrInf pstar, -1), -- never accept
                                                 (nanOrInf pi, 2)] $ error "metropolisLn: the impossible happend"
      in  do
        let (nextw, nj, nt) = calcNextW ada curw j t tt
        u <-  unitSample 
        xstar <- prop nextw ini xi
        let pstar = p (xstar,y) 
        let pi = p (xi,y) 
        return $ if u < accept par pi pstar
                      then Param (nj+1) (nt+1) (tt+1) pstar nextw ada ini xstar
                      else Param nj (nt+1) (tt+1) pi nextw ada ini xi
metSample2PC :: Show a => String -> (Double -> a -> a -> Sampler a) 
               -> PDF.PDF (a,b) -> (Param a,b) -> Sampler (Param a)
metSample2PC st prop p (par@(Param j t tt lhi curw ada ini xi),y) 
    = let accept xi pi pstar | notNanInf2 pi pstar =  min 1 $ exp (pstar - pi)
                             | otherwise = cond [(nanOrInf pi && nanOrInf pstar, 
                                                        error $ "metropolisLnP "++st++" pi pi pstar :"++
                                                                show (pi,pstar)++"\n"++
                                                                show xi),
                                                 (nanOrInf pstar, -1), -- never accept
                                                 (nanOrInf pi, 2)] $ error "metropolisLn: the impossible happend"
      in  do
        let (nextw, nj, nt) = calcNextW ada curw j t tt
        u <-  unitSample 
        xstar <- prop nextw ini xi
        let pstar = p (xstar,y) 
        let pi = if notNanInf lhi then lhi else p (xi,y)
        return $ if u < accept par pi pstar
                      then Param (nj+1) (nt+1) (tt+1) pstar nextw ada ini xstar
                      else Param nj (nt+1) (tt+1) pi nextw ada ini xi

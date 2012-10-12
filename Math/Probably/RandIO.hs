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

import qualified Data.Vector.Storable as V
import Statistics.Autocorrelation

import Text.Printf
import Data.List

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
       verboseNM :: Bool,
       amnsam :: Int,
       initw:: Int -> Double,
       minNM ::Int,
       maxNM ::Int }

defaultAM = AdaMetRunPars 0.5 Nothing False 1000 (const 0.02) 100 1000

traceit s x = trace (s++show x) x


getInitialCov :: Sampler (L.Vector Double)
              -> (L.Vector Double -> Double) 
              -> RIO (Double, L.Vector Double, L.Matrix Double)
getInitialCov inisam posteriorL = go 500 where 
  pdfv v = posteriorL v
  go n | n < 1 
         = error $ "can't find suitable initial conditions. Try a narrower prior"
       | otherwise =  do
    initialv <- sample inisam
    let iniSimplex = genInitial (negate . pdfv) [] (const 0.02)  $ initialv
    if any (nanOrInf . snd) iniSimplex
       then go (n-1)
       else runNM n iniSimplex
  runNM n iniSimplex = do  
    let finalSim = goNm (negate . pdfv) [] 1 100 1000 iniSimplex 
        (maxPost,hess) = hessianFromSimplex (negate . pdfv) [] [] finalSim 
    if any nanOrInf $ concat $ L.toLists hess
       then go (n-10)
       else getCov finalSim hess
  getCov finalSim hess = do
    let cor = case L.mbCholSH $ hess of 
            Just _ -> L.inv hess
            Nothing -> case L.mbCholSH $ PDF.posdefify hess of
                         Just _ -> L.inv $ PDF.posdefify hess
                         Nothing -> L.diag $ L.mapVector (recip) 
                                           $ L.takeDiag $ hess
        initV = fst $ head $ finalSim
    return (negate $ snd $ head $ finalSim, 
            initV, cor)

laplaceApprox :: AdaMetRunPars -> PDF.PDF (L.Vector Double) -> [Int] -> [((Int, Int), Double)] 
              -> L.Vector Double -> (L.Vector Double, Maybe (L.Matrix Double), Simplex)
laplaceApprox (AdaMetRunPars nmtol dispit verbnm nsam initw minNM maxNM) pdf isInt fixed init =
     let iniSim = genInitial (negate . pdf) isInt initw $ init
         finalSim = {-if verbnm then goNmVerbose (negate . pdf) isInt nmtol minNM maxNM iniSim 
                              else -} goNm (negate . pdf) isInt nmtol minNM maxNM iniSim 

         (maxPost,hess) = hessianFromSimplex (negate . pdf) isInt fixed finalSim 
--     io $ print maxPost
         mbcor = case L.mbCholSH $ hess of 
                   Just _ -> Just $ L.inv hess
                   Nothing -> case L.mbCholSH $ PDF.posdefify hess of
                                 Just _ -> Just $ L.inv $ PDF.posdefify hess
                                 Nothing -> Just $ L.diag $ L.mapVector (recip) $ L.takeDiag $ hess
         initV = fst $ head $ finalSim
     in (initV, mbcor, finalSim)

nmAdaMet :: AdaMetRunPars -> PDF.PDF (L.Vector Double) -> [Int] -> [((Int, Int), Double)] 
            -> L.Vector Double -> RIO [L.Vector Double]
nmAdaMet (AdaMetRunPars nmtol dispit verbnm nsam initw minNM maxNM ) pdf isInt fixed init = do
     let iniSim = genInitial (negate . pdf) isInt initw $ init
     io $ print iniSim
     let finalSim =  goNm (negate . pdf) isInt nmtol minNM maxNM iniSim
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
       _         -> do iniampar <- sample $ initialAdaMet 100 (const 5e-3) pdf initV
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
                                   putStrLn $ show (((n-nn) `div` chsz)*2)++"%: " ++showV (ampPar ampn) ++" LH="++printf "%.3g" (pdf (ampPar ampn)) ++ " accept="++acceptS ampn
                               go (nn-1) ns (ampn) $ (ampPar ampn):vs
           chsz = n `div` 50

runAdaMetRioESS :: Int -> Bool -> AMPar -> PDF.PDF (L.Vector Double) -> RIO [L.Vector Double]
runAdaMetRioESS want_ess freeze ampar pdf = do
    seed <- S.get
    (nseed, xs, amp) <- io $ go 200 seed ampar []
    S.put nseed
    return xs
     where go (0::Int) s amp vs = goChunks s amp vs
           go nn s amp vs = do let (!ampn, !ns) = unSam (adaMet freeze pdf amp) s
                               go (nn-1) ns (ampn) $ (ampPar ampn):vs
 
           goChunks s amp [] = do 
              (nseed, xs, namp) <- go 200 s amp []
              goChunks nseed namp xs
           goChunks s amp xs = do
              let have_ess = min (realToFrac $ count_accept amp) 
                                 $ calcESS  want_ess xs 
                  drawn = length xs
              putStrLn $ show $ ampPar amp
              putStrLn $ "ESS="++show have_ess++" from "++show drawn
                       ++" drawn accept ratio="++acceptS amp
              if have_ess > realToFrac want_ess
                 then return (s, xs, amp)
                 else do
                   let need_ess = min (realToFrac $ want_ess `div` 5) 
                                    $ max 1 
                                    $ realToFrac want_ess - have_ess
                       samples_per_es = realToFrac drawn/have_ess
                       to_do = max 50 $ min 2000 $ samples_per_es * need_ess
                   putStrLn $ "now doing "++ show to_do
                   go (round to_do) s amp xs 

runFixMetRioESS :: Int -> AMPar -> PDF.PDF (L.Vector Double) -> RIO [L.Vector Double]
runFixMetRioESS want_ess ampar pdf = do
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

runFixMetRioBurn :: Int -> AMPar -> PDF.PDF (L.Vector Double) -> RIO AMPar
runFixMetRioBurn burn ampar pdf = do
    seed <- S.get
    (nseed, amp) <- io $ go burn seed ampar
    S.put nseed
    return amp
     where go (0::Int) s amp  = return (s,amp)
           go nn s amp  = do let (!ampn, !ns) = unSam (fixedMet pdf amp) s
                             go (nn-1) ns (ampn) 
 
traceHead vs = trace ("head="++(show $ vs L.@> 0)) vs  

most_es_per_s = 10

calcESS :: Int -> [L.Vector Double] -> Double
calcESS  want_ess mcmcOut 
   | L.dim (head mcmcOut) <  want_ess * most_es_per_s * 2
      = calcESSprim mcmcOut
   | otherwise 
      = let thinFactor = L.dim (head mcmcOut) `div` ( want_ess * most_es_per_s)
        in calcESSprim $ map (thinV $ thinFactor - 1) mcmcOut

thinV 0   =  id
thinV thin = V.ifilter $ \ix _ -> ix `mod` thin == 0

--thinV thin v = V.generate ((V.length v `div` (thin+1))+1) $ \i -> (V.!) v  (i*(thin))


calcESSprim ::  [L.Vector Double] -> Double
calcESSprim mcmcOut = 
  let ndims = L.dim $ head mcmcOut
      len = realToFrac (length mcmcOut) 
      acfs = map (\i->  V.sum $ V.takeWhile (>0.1) $ fst3 $ autocorrelation $ V.fromList $ map (L.@>i) mcmcOut) [0..ndims-1]
      ess = foldl1' min $ flip map acfs $ \acfsum-> (len/(1+2*acfsum)) -- realToFrac samples/(1+2*acfsum)
  in ess

runAdaMetRIOtoFile :: Int -> Int -> String -> Bool -> AMPar -> PDF.PDF (L.Vector Double) -> RIO ()
runAdaMetRIOtoFile n thinn fileNm freeze ampar pdf = do
    seed <- S.get
    h <- io $ openFile (fileNm) WriteMode 
    (nseed) <- io $ go h n seed ampar
    S.put nseed
    return ()
    --return xs
     where go h 0 s amp  = do print $ amp
                              return s --  return (s, reverse vs)
           go h nn s amp = do let (!ampn, !ns) = unSam (adaMet freeze pdf amp) s
                              when(nn `rem` chsz==0) $ 
                                   putStrLn $ show (((n-nn) `div` chsz)*2)++"%: " ++
                                              showV (ampPar ampn) ++" LH="++
                                              printf "%.3g" (pdf (ampPar ampn)) ++ 
                                              " accept="++acceptS ampn
                              when (nn `rem` thinn == 0) $ do
                                   hPutStrLn h $ show $ L.toList $ ampPar ampn
                              go h (nn-1) ns (ampn) 
           chsz = n `div` 50


showV v = "<"++intercalate "," (map (printf "%.4g") $ L.toList v)++">"
acceptS ampar  | count ampar == 0 = "0/0"
               | otherwise = printf "%.3g" (rate::Double) ++ " ("++show yes++"/"++show total++")"where
   rate = realToFrac (yes) / realToFrac (total)
   yes = count_accept ampar
   total = count ampar

ampParRate ampar  | count ampar == 0 = 0
                  | otherwise = rate where
   rate = realToFrac (yes) / realToFrac (total)
   yes = count_accept ampar
   total = count ampar


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



bestOfTwoCov inisam posterior = do
   set1@(postval1,_,_) <- getInitialCov inisam posterior
   set2@(postval2,_,_) <- getInitialCov inisam posterior
   if postval1>postval2 -- I THINK
      then return set1
      else return set2


mkAMPar init cov initp = AMPar init init cov 2.4 initp 10 5

initAdaMetFromCov nsam pdf initv retries cov = do
  --lift $ putStrLn $ "starting from existing cov; try number "++ show retries
  iniampar <- sample $ initialAdaMetFromCov nsam (pdf) initv
                                                 (PDF.posdefify $ cov) 
  --lift $ print iniampar
  let rate = realToFrac (count_accept iniampar) / realToFrac nsam
  case () of
     _ | retries > 8 -> do lift $ putStrLn "initals ok." 
                           return iniampar
     _ | rate > 0.5 -> initAdaMetFromCov nsam pdf initv (retries +1) $ L.scale 2 cov 
     _ | rate > 0.40 -> initAdaMetFromCov nsam pdf initv (retries +1) $ L.scale 1.5 cov 
     _ | rate < 0.025 ->  initAdaMetFromCov nsam pdf initv (retries +1) $ L.scale 0.1 cov 
     _ | rate < 0.04 ->  initAdaMetFromCov nsam pdf initv (retries +1) $ L.scale 0.2 cov 
     _ | rate < 0.12 ->  initAdaMetFromCov nsam pdf initv (retries +1) $ L.scale 0.3 cov 
     _ | rate < 0.16 ->  initAdaMetFromCov nsam pdf initv (retries +1) $ L.scale 0.5 cov 
     _ | rate < 0.20 ->  initAdaMetFromCov nsam pdf initv (retries +1) $ L.scale 0.8 cov 
     _ | otherwise -> do lift $ putStrLn "initals ok." 
                         return iniampar  

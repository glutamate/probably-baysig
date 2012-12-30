{-# LANGUAGE RankNTypes, BangPatterns, ScopedTypeVariables, FlexibleInstances #-}

module Math.Probably.MALA where

import Math.Probably.MCMC
import qualified Math.Probably.PDF as PDF
import Math.Probably.RandIO
import Math.Probably.FoldingStats
import Math.Probably.Sampler
import qualified Math.Probably.PDF as PDF
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
import qualified Data.Vector.Storable as VS

import Debug.Trace
import Data.IORef
import Control.Spoon

class IsNaN a where
  isItNaN :: a -> Bool

instance IsNaN Double where
  isItNaN = nanOrInf

instance IsNaN (Vector Double) where
  isItNaN = any nanOrInf . toList

instance IsNaN (Matrix Double) where
  isItNaN = or . (map (any nanOrInf . toList)) . toRows

checkNaN :: IsNaN a => String -> a -> String
checkNaN s x | isItNaN x = " "++s++" "
             | otherwise = ""

class Covariance a where
  mvnSampler :: Vector Double ->  Double -> a -> Sampler (Vector Double)
  mvnPDF :: Vector Double -> Double -> a -> PDF.PDF (Vector Double)
  covMul :: a -> Vector Double -> Vector Double
  restrict :: Vector Int -> a -> a

instance Covariance (Matrix Double,Matrix Double,Matrix Double) where
  mvnSampler mu sigma (cov, covInv, covChol) 
     = multiNormalByChol mu (scale (sqrt sigma) covChol)
  mvnPDF mu sigma (cov, covInv, covChol) 
     = PDF.multiNormalByInvFixCov (scale (recip sigma) covInv) mu
  covMul (cov, covInv, covChol) v = cov <> v

instance Covariance (Vector Double) where
  mvnSampler mu sigma varv 
    = multiNormalIndep (scale sigma varv) mu
  mvnPDF mu sigma varv
    = PDF.multiNormalIndep (scale sigma varv) mu
  covMul varv v = VS.zipWith (*) varv  v
  restrict vixs v = VS.backpermute v vixs

data MalaPar = MalaPar { mpXi :: !(Vector Double),
                         mpPi :: !Double,
                         mpGradLast :: !(Vector Double),
                         mpSigma :: !Double,
                         mpCount :: !Double,
                         mpAccept :: !Double,
                         mpFreezeSigma :: !Bool,
                         mpThin :: !Int,
                         mpMaxGradLen :: !Double } --,
--                         mpLastRatio :: !Double }
  deriving Show

data Pair a b = Pair !a !b

data List a = Cons !a !(List a)
            | Nil

unPair (Pair x y) = (x,y)
unList (Cons x xs) = x: unList xs
unList Nil =[]

trunc t v = scale (t/(max t (norm2 v))) v

mala1 :: Covariance a => a -> (Vector Double -> (Double,Vector Double)) 
         -> Bool 
         -> MalaPar
         -> Sampler MalaPar
mala1 cov postgrad useCache
      (MalaPar xi piCached gradientiCached sigma tr tracc freeze thinn maxGrad) = do
  let (pi,gradienti) = if useCache 
                          then (piCached, gradientiCached)
                          else let (p, gr_untr) = postgrad xi
                               in (p, trunc maxGrad gr_untr)
  let xstarMean = xi + scale (sigma/2) (cov `covMul` gradienti)
  xstar <- mvnSampler xstarMean sigma cov
  u <- unitSample
  let (!pstar, gradientStarUntrunc) = postgrad xstar
      gradientStar = trunc maxGrad gradientStarUntrunc
  let !revJumpMean = xstar + scale (sigma/2) (cov `covMul` gradientStar)
      ptop = mvnPDF revJumpMean sigma cov xi
      pbot = mvnPDF xstarMean sigma cov xstar
      ratio = exp $   pstar -pi + ptop - pbot
      tr' = max 1 tr
      sigmaNext = case () of
         _ | freeze -> sigma
      accept = tracc / tr    
      freezeNext = freeze {-| freeze = True
                 | not freeze = tr > 100 && accept > 0.5 && accept < 0.6  -}
  if trace (show $ (tr, pstar ,pi , ratio, sigma)) $ u < ratio
     then return $ MalaPar xstar pstar (gradientStar) 
                           (if freezeNext then sigma else (min 1.4 $ 1+kmala/tr')*sigma) 
                           (tr+1) (tracc+1) freezeNext  thinn maxGrad
--                           sigma (tr+1) (tracc+1)
     else return $ MalaPar xi pi ( gradienti) 
                           (if freezeNext then sigma else (max 0.7143 $ 1-kmala/tr')**1.3*sigma) 
                           (tr+1) tracc freezeNext thinn maxGrad
--                           sigma (tr+1) tracc


blockMala1 :: Covariance a => 
  (Vector Double -> (Double,Vector Double)) ->
  Vector Double -> 
  [(a, MalaPar, Vector Int)] ->
  Sampler (Vector Double, [(a, MalaPar, Vector Int)])
blockMala1 postgrad xi blocks = do
  let go totalV [] accblocks = return (totalV, reverse accblocks)
      go totalV ((cov, mp0, vixs) : blocks) accblocks = do
          let myPostGrad vpars 
               = let (p, grad) = postgrad $ VS.update_ totalV vixs vpars
                 in (p, VS.backpermute grad vixs)             
          mp1 <- mala1 cov myPostGrad False mp0
          let newTotalV = VS.update_ totalV vixs (mpXi mp1)
          go newTotalV blocks $ (cov, mp1, vixs) : accblocks
  go xi blocks []
  
runMalaBlocks :: Covariance a => a -> (Vector Double -> (Double,Vector Double)) 
         ->  Int -> Double -> Int -> Vector Double -> [Vector Int] 
         -> RIO [Vector Double]
runMalaBlocks cov  postgrad nsam truncN thinN init  vixs = go nsam initBlocks init []  where
  initBlocks = flip map vixs $ \vix-> 
     (restrict vix cov, 
      MalaPar (VS.backpermute init vix) 0 (fromList [0]) 0.001 0 0 False thinN truncN,
      vix)
  go 0 _ _ vs = return vs
  go n blocks0 v0 vs = do
        (!v1, blocks1) <- sample $ blockMala1 postgrad v0 blocks0
        let (_,mpLast,_) = last blocks1 
        io $ print $ (n, mpPi mpLast, map (\(_,mp,_) -> mpSigma mp) blocks1)
        let newChainRes = if thinN == 0 || n `mod` thinN ==0
                             then v1 : vs
                             else vs
        go (n-1) blocks1 v1 newChainRes 


{-runMala :: Matrix Double -> (Vector Double -> (Double,Vector Double)) 
         ->  Int -> Vector Double -> RIO [(Double,Vector Double)]
runMala cov postgrad nsam init = go nsam mp1 [] where
  go 0 mpar xs = do io $ putStrLn $ "MALA accept = "++show (mpAccept mpar/mpCount mpar)
                    io $ putStrLn $ "MALA sigma = "++show (mpSigma mpar)
                    return xs
  go n y xs = do y1 <- sample $ mala1 cov postgrad y
                 go (n-1) y1 $ (mpPi y1, mpXi y1):xs 
  (pi, gradi) = postgrad init
  mp1 =  MalaPar init pi (gradi)  1 0 0 False -}

runMalaMP :: Covariance a => a -> (Vector Double -> (Double,Vector Double)) 
         ->  Int -> MalaPar -> [(Double,Vector Double)] -> RIO (MalaPar, [(Double,Vector Double)])
runMalaMP cov  pdf nsam init xs0 = go nsam init xs0 where
  go 0 mpar xs = do io $ putStrLn $ "MALA accept = "++show (mpAccept mpar/mpCount mpar)
                    io $ putStrLn $ "MALA sigma = "++show (mpSigma mpar)
                    return (mpar, xs)
  go n y xs = do y1 <- sample $ mala1 cov pdf True y
--                 io $ do putStrLn $ show (mpCount y1, mpPi y1, mpSigma y1)
--                         hFlush stdout
                 let newChainRes = if mpThin y1 == 0 || round (mpCount y1) `mod` mpThin y1 ==0
                                      then let !xi = mpXi y1
                                               !pi = mpPi y1
                                               !more = (pi,xi)
                                           in more:xs 
                                      else xs
                 go (n-1) y1 newChainRes

runMalaUntilBetter :: Covariance a => a ->(Vector Double -> (Double,Vector Double)) 
         ->  Int -> MalaPar -> RIO (MalaPar, [(Double,Vector Double)])
runMalaUntilBetter cov  pdf nsam init = go nsam init [] where
  pTarget = mpPi init
  go 0 mpar xs = do io $ putStrLn $ "MALA accept = "++show (mpAccept mpar/mpCount mpar)
                    io $ putStrLn $ "MALA sigma = "++show (mpSigma mpar)
                    return (mpar, xs)
  go n y xs = do y1 <- sample $ mala1 cov  pdf True y
                 io $ do putStrLn $ show (mpCount y1, mpPi y1, mpSigma y1)
                         hFlush stdout
                 let newChainRes = if mpThin y1 == 0 || round (mpCount y1) `mod` mpThin y1 ==0
                                      then (mpPi y1, mpXi y1):xs 
                                      else xs
                 if mpPi y1 > pTarget 
                    then return (y1, newChainRes)
                    else go (n-1) y1 $ newChainRes


{-runMalaMPaccept' :: Matrix Double -> (Vector Double -> (Double,Vector Double)) 
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
 
--  pi = pdf $ toList $ mpXi init -}

runMalaMPaccept :: Covariance a => a -> (Vector Double -> (Double,Vector Double)) 
         -> Int -> MalaPar ->  RIO (MalaPar, [(Double,Vector Double)])
runMalaMPaccept cov pdf nsam init = go init [] where
  go !mpar !xs
    | (round $ mpAccept mpar) >= nsam = do
         io $ putStrLn $ "MALA accept = "++show (mpAccept mpar/mpCount mpar)
         io $ putStrLn $ "MALA sigma = "++show (mpSigma mpar)
         return (mpar, xs)
    | mpAccept mpar < 0.5 && mpCount mpar > 20 = 
         return (mpar, [])
    | otherwise = do
         !mpar1 <- sample $ mala1 cov  pdf True mpar
         io $ putStr ((show (round $ mpAccept mpar)) ++".") >> hFlush stdout
         go mpar1 $ (mpPi mpar1, mpXi mpar1):xs
 
kmala = 5

{-runMalaRioESS ::  Matrix Double -> (Vector Double -> (Double,Vector Double)) 
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
               return  $ map snd  xs2 -}

runMalaRioCodaESS ::  Covariance a => a-> (Vector Double -> (Double,Vector Double)) 
                  ->  Int -> Double -> Vector Double -> RIO [Vector Double]
runMalaRioCodaESS cov pdf want_ess truncN xi = do
    let (p0,grad0) = pdf xi
    let sigma0 = 1.5 / (realToFrac $ dim xi) -- determined empirically
    let mp0 = MalaPar xi p0 (trunc truncN grad0) sigma0 0 0 False 0 truncN
        nsam0 = want_ess*1
    let converged mp  xs = do
         let have_ess = min (mpAccept mp) $ calcESSprim $ map snd xs
         io $ putStrLn $ "ESS=" ++show have_ess
         if have_ess > realToFrac want_ess
            then return $ map snd xs
            else do let need_ess =  max 1 $ realToFrac want_ess - have_ess
                        samples_per_es = realToFrac nsam0/have_ess
                        to_do = round $ samples_per_es * need_ess 
                    io $ putStrLn $ "running converged for "++show to_do
                    (mp2, xs2) <- runMalaMP cov pdf to_do (mp {mpFreezeSigma = True}) xs
                    io $ putStrLn $ "All done"
                    return $ map snd xs2 
    let go mp  n xs = do
            (mp2, xs2) <- runMalaMP cov  pdf n mp []
            let testres = mannWhitneyUtest TwoTailed 0.05 (U.fromList $ map fst xs2)
                                            (U.fromList $ map fst xs) 
              
            if testres/= Just NotSignificant
               then do io$ putStrLn $ "not converged: "++show testres++" at "++show (mpPi mp2)
                       go mp2  (round $ realToFrac n*2) xs2
               else do io$ putStrLn "converged!"
                       converged mp2  (xs2++xs)
    
    io $ putStrLn $ "initial sigma = "++show (mpSigma mp0)
    (mp1, xs1) <- runMalaMPaccept cov  pdf nsam0 mp0 
    case xs1 of
            [] -> return []
            _ -> go mp1  (round $ mpCount mp1) xs1

{-    case (spoon (invlndet cov), mbCholSH cov) of
       (Just (covInv, (lndt,_)), Just covChol) -> go_rest (covInv, covChol)
       _ -> let cov' = PDF.posdefify cov in 
            case (spoon (invlndet cov'), mbCholSH cov') of
              (Just (covInv, (lndt,_)), Just covChol) -> go_rest (covInv, covChol)
              _ ->  do io $ putStrLn "non-invertible covariance matrix"
                       return [] -}

runMalaRioSimple ::  Covariance a => a -> (Vector Double -> (Double,Vector Double)) 
                  ->  Int -> Double -> Int -> Vector Double -> RIO [Vector Double]
runMalaRioSimple cov pdf samples maxGrad thinN xi = do
    let (p0,grad0) = pdf xi
    let sigma0 = 2.7e-4 --1.5 / (realToFrac $ dim xi) -- determined empirically
    let mp0 = MalaPar xi p0 (trunc maxGrad grad0) sigma0 0 0 False thinN maxGrad
    
    (mp2, xs2) <- runMalaMP cov  pdf samples mp0 []
    io $ putStrLn $ "All done"
    return $ map snd xs2 

calcCovariance :: Vector Double -> 
                  Vector Double -> 
                  (Vector Double -> (Double,Vector Double)) ->
                  (Vector Double -> Double) -> 
                  Either (Vector Double) (Matrix Double, Matrix Double, Matrix Double)
calcCovariance vinit vnear postgrad posterior = finalcov where
   ndim = dim vinit
   finalcov 
     | ndim > 0 -- > 20000 --FIXME 
        = Left $ calcFDindepVars vinit vnear posterior --Left $ iCov vinit -- 
     | otherwise 
        = hessToCov (calcFDhess vinit vnear postgrad) Nothing

iCov v = VS.replicate (VS.length v) 1 -- in (m,m,m)

calcFDindepVars v v' post = trace ("FDVars = "++show (VS.take 10 vars))  $ vars where
   hv =  mapVector (*1e-4) v --mapVector (max 1e-9 .  abs) $ v - v'
   n = dim v
   postv = post v
   postPlus i = post $ v VS.// [(i,v @>i + hv @> i)]
   postMinus i = post $ v VS.// [(i,v @>i - hv@> i)]
   vars = buildVector n fvar
   fvar i = negate $ recip $ (postPlus i - 2*postv + postMinus i)/((hv @> i)*(hv @> i))


calcFDhess v v' postgrad = hess2 where
   grad =  snd . postgrad
   gradi i = (@>i) . grad
   hv = mapVector (max 1e-9 . abs) $ v - v'
   n = dim v
   gradv = grad v
   grads =  fromRows $ flip map [0..(n-1)] $ \i -> 
               grad (v VS.// [(i,v @>i + hv @> i)])
   fhess (i,j) | i<j = 0
               | otherwise = (grads @@>(j,i) - (gradv @> i))
                                       /(2*(hv @> j)) +
                             (grads @@>(i,j)- (gradv @> j))
                                       /(2*(hv @> i))
                 
--   hess3 = scale (recip $ realToFrac n) $ sum $ map outerSelf $ grads                      
                             
   hess1 = buildMatrix n n fhess 
   hess2 = buildMatrix n n $ \(i,j) -> if i>=j then hess1 @@> (i,j)
                                               else hess1 @@> (j,i)
 

outerSelf v= v `outer` v

hessToCov hess mOriginal = 
   let mTryPosdefify = case mOriginal of 
            Nothing -> hessToCov (PDF.posdefify hess) (Just hess)
            Just horiginal -> let ds =  mapVector (negate . recip) 
                                         $ takeDiag horiginal 
                              in trace ("USING DIAGS"++show (VS.take 10 ds)) $ Left $ ds
   in
   case spoon $ inv $ negate $ hess of
     Just cov -> case mbCholSH cov of
                   Just cholm ->  trace ("invert success:"++show (VS.take 10 $ takeDiag cov) ++ "det="++show (det cov)) $ Right (cov, negate hess, cholm)
                   Nothing -> trace ("chol fail") mTryPosdefify
     Nothing -> trace ("inv fail") mTryPosdefify
                  {- Just cov -> case mbCholSH cov of
                                Just cholm ->  Right (cov, negate hessToCov,cholm)
                                Nothing -> case mbCholSH $ PDF.posdefify cov of
                                            Just cholm -> Right (cov, negate hess,cholm)
                                            Nothing -> vars
                  Nothing -> vars -}

acceptSM ampar  | mpCount ampar == 0 = "0/0"
               | otherwise = printf "%.3g" (rate::Double) ++ " ("++show yes++"/"++show total++")" where
   rate = realToFrac (yes) / realToFrac (total)
   yes = mpAccept ampar
   total = mpCount ampar
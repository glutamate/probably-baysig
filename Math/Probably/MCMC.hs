{-# LANGUAGE Arrows, ViewPatterns #-}

module Math.Probably.MCMC where

import Math.Probably.Sampler 
import qualified Math.Probably.PDF as P
import Math.Probably.StochFun
import Control.Arrow
import Data.List
import Data.Maybe
import Data.Ord
import Math.Probably.FoldingStats
--import TNUtils
import Control.Monad
import Debug.Trace
import Data.Binary
import qualified Numeric.LinearAlgebra as L
import qualified Data.Packed.Matrix as L


--http://videolectures.net/mlss08au_freitas_asm/
rejection :: Double -> P.PDF a -> Sampler a -> P.PDF a -> Sampler a
rejection mult nicePDF niceSampler nastyPDF = rej  
    where rej = do  x <- niceSampler
                    u <- unitSample
                    if u < (nastyPDF x)/(mult * (nicePDF x))
                       then return x
                       else rej


importanceIO :: Fractional b => P.PDF a -> Sampler a -> (a->b) -> P.PDF a -> IO [b]
importanceIO nicePDF niceSampler f nastyPDF = do
  let markov = mvSampler (importance nicePDF niceSampler f nastyPDF) 
  means `fmap` runMarkovIO markov 

--dont read too much into the type
importance :: Fractional b => P.PDF a -> Sampler a -> (a->b) -> P.PDF a -> Sampler b
importance nicePDF niceSampler f nastyPDF = do
  x <- niceSampler
  return $ (f x)*(realToFrac $ (nastyPDF x)/(nicePDF x) )
                 
--andrieu intro mcmc for ml p 16 fig 5
metropolisHastings :: (a-> P.PDF a) -> (a->Sampler a) -> P.PDF a -> StochFun a a
metropolisHastings qPDF qSam p 
    = let accept xi xstar = min 1 $ (p xstar * qPDF xstar xi)/(p xi * qPDF xi xstar)
      in proc xi -> do
        u <- sampler unitSample -< ()
        xstar <- condSampler qSam -< xi
        returnA -< if u < accept xi xstar
                      then xstar
                      else xi


metropolis :: (a->Sampler a) -> P.PDF a -> StochFun a a
metropolis qSam p 
    = let accept xi xstar = min 1 $ (p xstar)/(p xi)
      in proc xi -> do
        u <- sampler unitSample -< ()
        xstar <- condSampler qSam -< xi
        returnA -< if u < accept xi xstar
                      then xstar
                      else xi

metSample1 ::  Show a => (a->Sampler a) -> P.PDF a -> a -> Sampler a
metSample1 prop pdf = uncondSampler $ metropolisLn prop pdf

notNanInf x = not (isNaN x) && not (isInfinite x)

notNanInf2 x y = notNanInf x && notNanInf y
nanOrInf x = isNaN x || isInfinite x

metropolisLn :: Show a => (a->Sampler a) -> P.PDF a -> StochFun a a
metropolisLn qSam p 
    = let accept xi pi pstar | notNanInf2 pi pstar =  min 1 $ exp (pstar - pi)
                             | otherwise = cond [(nanOrInf pi && nanOrInf pstar, 
                                                        error $ "metropolisLn pi pstar :"++show (pi,pstar)++"\n"++
                                                                show xi),
                                                 (nanOrInf pstar, -1), -- never accept
                                                 (nanOrInf pi, 2)] $ error "metropolisLn: the impossible happend"
      in proc (xi) -> do
        u <- sampler unitSample -< ()
        xstar <- condSampler qSam -< xi
        let pstar = p xstar
        let pi = p xi
        returnA -< if u < accept xi pi pstar
                      then xstar
                      else xi

metropolisHastingsLn :: (a-> P.PDF a) -> (a->Sampler a) -> P.PDF a -> StochFun a a
metropolisHastingsLn qPDF qSam p 
    = let accept xi xstar = min 1 $ exp (p xstar - qPDF xstar xi - p xi + qPDF xi xstar)
      in proc xi -> do
        u <- sampler unitSample -< ()
        xstar <- condSampler qSam -< xi
        returnA -< if u < accept xi xstar
                      then xstar
                      else xi


data Param a = Param { jumpCount :: !Int,
                       totalCount :: !Int,
                       totalTotalCount :: !Int,
                       cachedLH :: Double,
                       currentWidth :: !Double,
                       adaptive :: !Int,
                       initial :: !a,
                       unP :: !a } 
               | NonInitialisedParam deriving Show

instance Binary a => Binary (Param a) where
    put (Param j t tt cLH curW ada ini x) = 
      put (j,t, tt, cLH, curW , ada ,ini, x)
    get = do
      (j,t, tt, cLH, curW, ada, ini, x) <- get
      return $ Param j t tt cLH curW ada ini x


newParam :: a -> Param a
newParam x = Param 0 0 0 (1/0) 1 200 x x

condDepSampler :: (Double -> b-> b->Sampler a) -> StochFun (Double,b,b) a
condDepSampler dqSam = SF $ \((w,ini,x),dbls) -> unSam (dqSam w ini x) dbls

metSample1P :: Show a => String -> (Double -> a -> a -> Sampler a) -> P.PDF a -> (Param a) -> Sampler (Param a)
metSample1P st prop pdf = uncondSampler $ metropolisLnP st prop pdf

--metSample1PCL :: Show a => String -> (Double -> a -> a -> Sampler a) -> P.PDF a -> P.PDF a-> (Param a) -> Sampler (Param a)
--metSample1PCL st prop lh prior = uncondSampler $ metropolisLnPCL st prop lh prior


metropolisLnP ::  Show a => String -> (Double -> a-> a-> Sampler a) ->  P.PDF a -> StochFun (Param a) (Param a)
metropolisLnP st qSam p
    = let accept xi pi pstar | notNanInf2 pi pstar =  min 1 $ exp (pstar - pi)
                             | otherwise = cond [(nanOrInf pi && nanOrInf pstar, 
                                                        error $ "metropolisLnP "++st++" pi pi pstar :"++
                                                                show (pi,pstar)++"\n"++
                                                                show xi),
                                                 (nanOrInf pstar, -1), -- never accept
                                                 (nanOrInf pi, 2)] $ error "metropolisLn: the impossible happend"
      in proc par@(Param j t tt _ curw ada ini xi) -> do
        let (nextw, nj, nt) = calcNextW ada curw j t tt
        u <- sampler unitSample -< ()
        xstar <- condDepSampler qSam -< (nextw, ini, xi)
        let pstar = p xstar 
        let pi = p xi 
        returnA -< if u < accept par pi pstar
                      then Param (nj+1) (nt+1) (tt+1) pstar nextw ada ini xstar
                      else Param nj (nt+1) (tt+1) pi nextw ada ini xi

metropolisLnP2 ::  Show a => String -> (Double -> a-> a-> Sampler a) ->  (a -> a-> Double) -> StochFun (Param a) (Param a)
metropolisLnP2 st qSam p2
    = let accept xi pdiff    | notNanInf pdiff =  exp pdiff
                             | otherwise = -1 
      in proc par@(Param j t tt _ curw ada ini xi) -> do
        let (nextw, nj, nt) = calcNextW ada curw j t tt
        u <- sampler unitSample -< ()
        xstar <- condDepSampler qSam -< (nextw, ini, xi)
        let pdiff = p2 xstar xi
        returnA -< if u < accept par pdiff
                      then Param (nj+1) (nt+1) (tt+1) pdiff nextw ada ini xstar
                      else Param nj (nt+1) (tt+1) pdiff nextw ada ini xi

greedyLnP2 :: Show a => String -> (Double -> a -> a -> Sampler a) -> (a -> a-> Double) -> StochFun (Param a) (Param a)
greedyLnP2 st qSam p2 =
      proc par@(Param j t tt lhi curw ada ini xi) -> do
        u <- sampler unitSample -< ()
        xstar <- condDepSampler qSam -< (curw, ini, xi)
        let pdiff = p2 xstar xi
        --let pi = priorf xi + lhi
        returnA -< if pdiff>0 -- u < accept par pi pstar
                      then Param j t (tt) pdiff curw ada ini xstar
                      else Param j t (tt) pdiff    curw ada ini xi

x `divides` y = y `mod` x == 0

calcNextW 0 w j t _ = (w, j, t)
calcNextW mutFreq w j t tt | mutFreq `divides` t= 
                     let jf = realToFrac j / realToFrac t 
                         nxtW = w*nextW jf in                    
                    {-trace (show (w, nxtW, j, t)) -} (nxtW, 0, 0)
                   | otherwise = (w, j, t)
{-    where mutFreq = cond [(t<20000, 2000),
                          (t<200000, 5000)] 5000000 -}
                       

nextW jf | jf > 0.80 = 3
         | jf > 0.50 = 1.4
         | jf < 0.05 = 0.3
         | jf < 0.10 = 0.6
         | jf < 0.20 = 0.8
         | otherwise = 1

{-metropolisLnPCL :: Show a => String -> (Double -> a -> a -> Sampler a) -> P.PDF a -> P.PDF a -> StochFun (Param a) (Param a)
metropolisLnPCL st qSam lhf priorf 
    = let accept xi pi pstar | notNanInf2 pi pstar =  min 1 $ exp (pstar - pi)
                             | otherwise = cond [(nanOrInf pi && nanOrInf pstar,
                                                        error $ "metropolisLnPCL "++st++" pi pi pstar :"++
                                                                show (pi,pstar)++"\n"++
                                                                show xi),
                                                (nanOrInf pstar, -1), -- never accept                                
                                                (nanOrInf pi, 2)] $ error "metropolisLn: the impossible happend"
      in proc par@(Param j t tt lhi curw ini xi) -> do
        let (nextw, nj, nt) = calcNextW curw j t tt
        u <- sampler unitSample -< ()
        xstar <- condDepSampler qSam -< (nextw, ini, xi)
        let lhstar = lhf xstar
        let lhi' = if notNanInf lhi then lhi else lhf xi
        let pstar = priorf xstar + lhstar
        let pi = priorf xi + lhi
        returnA -< if u < accept par pi pstar
                      then Param (nj+1) (nt+1) (tt+1) lhstar nextw ini xstar
                      else Param nj (nt+1) (tt+1) lhi nextw ini xi
-}
metropolisLnPC :: Show a => String -> (Double -> a -> a -> Sampler a) -> P.PDF a -> StochFun (Param a) (Param a)
metropolisLnPC st qSam pdf
    = let accept xi pi pstar | notNanInf2 pi pstar =  min 1 $ exp (pstar - pi)
                             | otherwise = cond [(nanOrInf pi && nanOrInf pstar,
                                                        error $ "metropolisLnPC "++st++" pi pi pstar :"++
                                                                show (pi,pstar)++"\n"++
                                                                show xi),
                                                (nanOrInf pstar, -1), -- never accept                                
                                                (nanOrInf pi, 2)] $ error "metropolisLn: the impossible happend"
      in proc par@(Param j t tt lhi curw ada ini xi) -> do
        let (nextw, nj, nt) = calcNextW ada curw j t tt
        u <- sampler unitSample -< ()
        xstar <- condDepSampler qSam -< (nextw, ini, xi)
        let pi = if notNanInf lhi then lhi else pdf xi
        let pstar = pdf xstar 
        --let pi = priorf xi + lhi
        returnA -< if u < accept par pi pstar
                      then Param (nj+1) (nt+1) (tt+1) pstar nextw ada ini xstar
                      else Param nj (nt+1) (tt+1) pi nextw ada ini xi
greedyLnPC :: Show a => String -> (Double -> a -> a -> Sampler a) -> P.PDF a -> StochFun (Param a) (Param a)
greedyLnPC st qSam pdf =
      proc par@(Param j t tt lhi curw ada ini xi) -> do
        u <- sampler unitSample -< ()
        xstar <- condDepSampler qSam -< (curw, ini, xi)
        let pi = if notNanInf lhi then lhi else pdf xi
        let pstar = pdf xstar 
        --let pi = priorf xi + lhi
        returnA -< if pstar > pi -- u < accept par pi pstar
                      then Param j t (tt) pstar curw ada ini xstar
                      else Param j t (tt) pi    curw ada ini xi



traceIt :: Show a => a -> a
traceIt x = trace (show x) x

metropolisLog ::(a->Sampler a) -> P.PDF a -> StochFun (a,Double) (a,Double)
metropolisLog qSam p 
    = let accept pi pstar =  min 1 $ exp (pstar - pi)
      in proc (xi, pi) -> do
        u <- sampler unitSample -< ()
        xstar <- condSampler qSam -< xi
        let pstar = p xstar
        returnA -< if u < accept pi pstar
                      then (xstar, pstar)
                      else (xi, pi)

samplingImportanceResampling :: Ord a => [(a,Double)] -> Sampler a
samplingImportanceResampling weightedSamples = 
  let sumWeights = sum $ map snd weightedSamples
      cummWeightedSamples = scanl (\(_,csum) (x,w) -> (x,csum+w)) (undefined,0) $ sortBy (comparing fst) weightedSamples
  in do
    u <- unitSample
    return . fst . fromJust $ find ((>=u*sumWeights) . snd) cummWeightedSamples
  
abcRej :: (th -> Sampler obs) -> (obs -> obs -> Bool) -> obs -> Sampler th -> Sampler th
abcRej  likelihood accept theData prior = abcrej
    where abcrej = do
            suggest <- prior
            sim <- likelihood suggest
            if accept sim theData
               then return suggest
               else abcrej

--parameter 
bayes :: Ord a => Int -> P.PDF a -> Sampler a -> IO (Sampler a)
bayes nsam likelihood prior = do
  let postsam = do
        theta <- prior
        let lh_theta = likelihood theta
        return (theta, lh_theta)
  weightedSamples <- take nsam `fmap` runSamplerIO postsam
  return $ samplingImportanceResampling weightedSamples

bayesMet :: (a->Sampler a) -> P.PDF a -> P.PDF a -> StochFun a a
bayesMet proposal lh prior = metropolis proposal (\x-> lh x * prior x)

bayesMetLog :: Show a => (a->Sampler a) -> [P.PDF a] -> a -> Markov a
bayesMetLog proposal pdfs inits = 
    let p x =  sum $ map ($x) pdfs
        p0 = p inits
    in Mrkv (metropolisLog proposal p) (inits, p0) (fst)


bayesMetHastLog :: Show a => (a->P.PDF a) -> (a->Sampler a) -> P.PDF a -> a -> Markov a
bayesMetHastLog propPDF proposal p inits = 
    {-let p0 = p inits
    in-} Mrkv (metropolisHastingsLn propPDF proposal p) (inits) (id)

--blockMetropolis :: Show a => (Double -> a-> a->Sampler a) -> P.PDF a -> a -> Markov a
{-blockMetropolis :: Show a => (a->Sampler a) -> P.PDF a -> a -> Markov a
blockMetropolis proposal pdf inits = 
--   Mrkv (metropolisLnPC "" proposal pdf) (newParam inits) (unParam)
   Mrkv (metropolisLn proposal pdf) (inits) id -}

--unParam (Param j t tt cLH curW _ ini x) = x

stopAdaptation p = p { adaptive = 0 } 

manyLike :: (theta -> a -> P.PDF b) -> ([(a,b)] -> P.PDF theta)
manyLike lh1 = \xys -> \theta -> product $ map (\(x,y) -> lh1 theta x y) xys


{-test = 
  let xs = [1, 2, 3]
      ys = [2, 3.9, 6.1]
      lh (a, b, sd) x = P.gauss (a*x+b) sd
      prior = do a <- gauss (3) 0.5
                 b <- gauss (0) 0.5
                 sd <- uniform 0 5
                 return (a,b,sd)
  in do bsam <- bayes 10000 (manyLike lh $ zip xs ys) prior
        ps <- take 1000 `fmap` runSamplerIO bsam
        print $ meanSDF `runStat` (map fst3 ps)
        print $ meanSDF `runStat` (map snd3 ps)
        print $ regressF `runStat`  zip xs ys -}

cond :: [(Bool, a)] -> a -> a
cond [] x = x
cond ((True, x):_) _ = x
cond ((False, _):conds) x = cond conds x

class MutateGaussian a where
    mutGauss :: Double -> a -> Sampler a
    mutGauss cv x = mutGaussAbs x cv x
    mutGaussAbs :: a -> Double -> a -> Sampler a
    --mutGaussAbs _ = mutGauss
    mutGaussMany :: Double -> [a] -> Sampler [a]
    mutGaussMany cv = mapM (mutGauss cv) 
    nearlyEq :: Double -> a -> a -> Bool

instance MutateGaussian Double where
    mutGauss cv x = gaussD x (cv*x)
    mutGaussAbs x0 cv x = gaussD x (cv*x0)
    mutGaussMany cv xs = gaussManyD (map (\x-> (x,cv*x)) xs)
    nearlyEq tol x y = abs(x-y)<tol  

instance MutateGaussian Int where
    mutGaussAbs _ cv' x = do
      u <- unitSample
      let cv = 0.5 -- max 0 $ min 0.4 (1/cv')
      case u of 
        _ | u < 0.3 -> return $ x-1
          | u > 0.7 -> return $ x+1
          | otherwise -> return $ x
    nearlyEq _ x y = x==y

{-instance MutateGaussian Int where
    mutGauss cv x = round `fmap` gaussD (realToFrac x) (cv*realToFrac x)
    nearlyEq tol x y = x==y -}

instance MutateGaussian a => MutateGaussian [a] where
    mutGauss cv xs = mutGaussMany cv xs 
    mutGaussAbs xs0 cv xs =  mapM (\(x0,x)-> mutGaussAbs x0 cv x) $ zip xs0 xs
    nearlyEq tol xs ys = length xs == length ys && (all (uncurry $ nearlyEq tol) $ zip xs ys )

instance (MutateGaussian a, MutateGaussian b) => MutateGaussian (a,b) where
    mutGauss cv (x,y) = liftM2 (,) (mutGauss cv x) (mutGauss cv y)
    mutGaussAbs (x0, y0) cv (x,y) = liftM2 (,) (mutGaussAbs x0 cv x) (mutGaussAbs y0 cv y)
    nearlyEq t (x,y) (x1,y1) = nearlyEq t x x1 && nearlyEq t y y1

instance (MutateGaussian a, MutateGaussian b, MutateGaussian c) => MutateGaussian (a,b,c) where
    mutGauss cv (x,y,z) = liftM3 (,,) (mutGauss cv x) (mutGauss cv y) (mutGauss cv z)
    mutGaussAbs (x0, y0, z0) cv (x,y,z) = 
        liftM3 (,,) (mutGaussAbs x0 cv x) (mutGaussAbs y0 cv y) (mutGaussAbs z0 cv z)
    nearlyEq t (x,y, z) (x1,y1, z1) = nearlyEq t x x1 && nearlyEq t y y1 && nearlyEq t z z1

instance (MutateGaussian a, MutateGaussian b, MutateGaussian c, MutateGaussian d) => MutateGaussian (a,b,c,d) where
    mutGauss cv (x,y,z,w) = liftM4 (,,,) (mutGauss cv x) (mutGauss cv y) (mutGauss cv z) (mutGauss cv w)
    mutGaussAbs (x0, y0, z0, w0) cv (x,y,z,w) = 
        liftM4 (,,,) (mutGaussAbs x0 cv x) (mutGaussAbs y0 cv y) (mutGaussAbs z0 cv z) (mutGaussAbs w0 cv w)
    nearlyEq t (x,y, z, w) (x1,y1, z1, w1) = nearlyEq t x x1 && nearlyEq t y y1 && nearlyEq t z z1 && nearlyEq t w w1

instance (MutateGaussian a1, 
          MutateGaussian a2, 
          MutateGaussian a3, 
          MutateGaussian a4, 
          MutateGaussian a5) 
          => MutateGaussian (a1,a2,a3,a4,a5) where
    mutGaussAbs (x0, y0, z0, w0,w0') cv (x,y,z,w,w') = 
        liftM5 (,,,,) (mutGaussAbs x0 cv x) 
                      (mutGaussAbs y0 cv y) 
                      (mutGaussAbs z0 cv z)
                      (mutGaussAbs w0 cv w)
                      (mutGaussAbs w0' cv w')
    nearlyEq t (x,y, z, w, w') 
               (x1,y1, z1, w1,w1') 
             =    nearlyEq t x x1 
               && nearlyEq t y y1 
               && nearlyEq t z z1 
               && nearlyEq t w w1 
               && nearlyEq t w' w1'

instance (MutateGaussian a1, 
          MutateGaussian a2, 
          MutateGaussian a3, 
          MutateGaussian a4, 
          MutateGaussian a5, 
          MutateGaussian a6) 
          => MutateGaussian (a1,a2,a3,a4,a5,a6) where
    mutGaussAbs (x0, y0, z0, w0,w0',s0) cv (x,y,z,w,w',s) = 
        return (,,,,,) `ap` (mutGaussAbs x0 cv x) 
                       `ap` (mutGaussAbs y0 cv y) 
                       `ap` (mutGaussAbs z0 cv z)
                       `ap` (mutGaussAbs w0 cv w)
                       `ap` (mutGaussAbs w0' cv w')
                       `ap` (mutGaussAbs s0 cv s)
    nearlyEq t (x,y, z, w, w',s) 
               (x1,y1, z1, w1,w1',s1) 
             =    nearlyEq t x x1 
               && nearlyEq t y y1 
               && nearlyEq t z z1 
               && nearlyEq t w w1 
               && nearlyEq t w' w1'
               && nearlyEq t s s1

instance (MutateGaussian a1, 
          MutateGaussian a2, 
          MutateGaussian a3, 
          MutateGaussian a4, 
          MutateGaussian a5, 
          MutateGaussian a6, 
          MutateGaussian a7) 
          => MutateGaussian (a1,a2,a3,a4,a5,a6,a7) where
    mutGaussAbs (x0, y0, z0, w0,w0',s0,t0) cv (x,y,z,w,w',s,t) = 
        return (,,,,,,) `ap` (mutGaussAbs x0 cv x) 
                       `ap` (mutGaussAbs y0 cv y) 
                       `ap` (mutGaussAbs z0 cv z)
                       `ap` (mutGaussAbs w0 cv w)
                       `ap` (mutGaussAbs w0' cv w')
                       `ap` (mutGaussAbs s0 cv s)
                       `ap` (mutGaussAbs t0 cv t)
    nearlyEq t (x,y, z, w, w',s,tv) 
               (x1,y1, z1, w1,w1',s1,tv1) 
             =    nearlyEq t x x1 
               && nearlyEq t y y1 
               && nearlyEq t z z1 
               && nearlyEq t w w1 
               && nearlyEq t w' w1'
               && nearlyEq t s s1
               && nearlyEq t tv tv1

instance (MutateGaussian a1, 
          MutateGaussian a2, 
          MutateGaussian a3, 
          MutateGaussian a4, 
          MutateGaussian a5, 
          MutateGaussian a6, 
          MutateGaussian a7,
          MutateGaussian a8) 
          => MutateGaussian (a1,a2,a3,a4,a5,a6,a7,a8) where
    mutGaussAbs (x0, y0, z0, w0,w0',s0,t0,r0) cv (x,y,z,w,w',s,t,r) = 
        return (,,,,,,,) `ap` (mutGaussAbs x0 cv x) 
                       `ap` (mutGaussAbs y0 cv y) 
                       `ap` (mutGaussAbs z0 cv z)
                       `ap` (mutGaussAbs w0 cv w)
                       `ap` (mutGaussAbs w0' cv w')
                       `ap` (mutGaussAbs s0 cv s)
                       `ap` (mutGaussAbs t0 cv t)
                       `ap` (mutGaussAbs r0 cv r)
    nearlyEq t (x,y, z, w, w',s,tv,r) 
               (x1,y1, z1, w1,w1',s1,tv1,r1) 
             =    nearlyEq t x x1 
               && nearlyEq t y y1 
               && nearlyEq t z z1 
               && nearlyEq t w w1 
               && nearlyEq t w' w1'
               && nearlyEq t s s1
               && nearlyEq t tv tv1
               && nearlyEq t r r1

instance (MutateGaussian a1, 
          MutateGaussian a2, 
          MutateGaussian a3, 
          MutateGaussian a4, 
          MutateGaussian a5, 
          MutateGaussian a6, 
          MutateGaussian a7,
          MutateGaussian a8, 
          MutateGaussian a9) 
          => MutateGaussian (a1,a2,a3,a4,a5,a6,a7,a8,a9) where
    mutGaussAbs (x0, y0, z0, w0,w0',s0,t0,r0,q0) cv (x,y,z,w,w',s,t,r,q) = 
        return (,,,,,,,,) `ap` (mutGaussAbs x0 cv x) 
                       `ap` (mutGaussAbs y0 cv y) 
                       `ap` (mutGaussAbs z0 cv z)
                       `ap` (mutGaussAbs w0 cv w)
                       `ap` (mutGaussAbs w0' cv w')
                       `ap` (mutGaussAbs s0 cv s)
                       `ap` (mutGaussAbs t0 cv t)
                       `ap` (mutGaussAbs r0 cv r)
                       `ap` (mutGaussAbs q0 cv q)
    nearlyEq t (x,y, z, w, w',s,tv,r,q) 
               (x1,y1, z1, w1,w1',s1,tv1,r1,q1) 
             =    nearlyEq t x x1 
               && nearlyEq t y y1 
               && nearlyEq t z z1 
               && nearlyEq t w w1 
               && nearlyEq t w' w1'
               && nearlyEq t s s1
               && nearlyEq t tv tv1
               && nearlyEq t r r1
               && nearlyEq t q q1

writeInChunks ::  String -> Int ->   [[Double]] -> IO ()
writeInChunks = writeInChunks' 0
    where writeInChunks' _ _ _  [] = return ()
          writeInChunks' counter fnm chsize xs = do
            let (out, rest) = splitAt chsize xs
            writeFile (fnm++"_file"++(show counter)++".mcmc") $ unlines $ map show out
            writeInChunks' (counter+1) fnm chsize rest


--- Adaptive Metropolis from Haario et al

data AMPar = AMPar { ampPar :: !(L.Vector Double),
                     ampMean :: !(L.Vector Double),
                     ampCov :: !(L.Matrix Double),
                     count :: !Int } deriving Show

empiricalCovariance :: [L.Vector Double] -> L.Matrix Double
empiricalCovariance xs
   = let k = length xs 
         --barxk = L.scale (recip $ realToFrac $ k+1) $ sum xs
         --m1 = sum $ map (\xv-> L.outer xv xv) xs
         --m2 = L.scale (realToFrac $ k+1) $ L.outer barxk barxk
         xmn = empiricalMean xs
         f xi = outerSame $ xi - xmn
--     in L.scale (recip $ realToFrac k) $ m1 - m2
     in L.scale (recip $ realToFrac k) $ sum $ map f xs

outerSame v = L.outer v v

empiricalMean :: [L.Vector Double] -> L.Vector Double
empiricalMean vecs = L.scale (recip $ realToFrac $ length vecs) $ sum vecs

initialAdaMet :: Int -> P.PDF (L.Vector Double) -> 
                 L.Vector Double -> Sampler AMPar
initialAdaMet n pdf  init = do
              let propose cur = fmap ( L.fromList) $ 
                                forM (zip (L.toList cur) (L.toList init)) 
                                     $ \(x,ini) -> gaussD x (ini*5e-3)
              let oneStep xi = do
                    xstar <- propose xi
                    u <- unitSample
                    let pi = pdf xi
                    let pstar = pdf xstar
                    return $ if u < exp (pstar - pi)
                                then {-trace ("IACCEPT "++show xstar) -} xstar
                                else {-trace ("IREJECT "++show xstar) -} xi   
              vecs <- runChainS n init oneStep
              let cov = empiricalCovariance vecs              
              let mn = empiricalMean vecs
              return $ AMPar (last vecs) mn cov n
     
runChainS :: Int -> a -> (a -> Sampler a) -> Sampler [a]
runChainS 0 _ _ = return []
runChainS n x sam = do 
       y <- sam x
       ys <- runChainS (n-1) y sam
       return $ y:ys

runAdaMet :: Int -> Bool -> P.PDF (L.Vector Double) -> AMPar -> Sampler [L.Vector Double]
runAdaMet 0 freeze _ amp   = return []
runAdaMet n freeze pdf amp = do 
       amp' <- adaMet freeze pdf amp
       ys <- runAdaMet (n-1) freeze pdf amp'
       return $ ampPar amp':ys

adaMet :: Bool -> P.PDF (L.Vector Double) -> 
          AMPar -> Sampler AMPar 
adaMet freeze pdf (AMPar xi mn cov (realToFrac -> t)) = do
   --propose
   let dims = L.dim xi
   let scalar = (2.4*2.4/realToFrac dims)::Double
   xstar <-  multiNormal xi (L.scale (scalar::Double) cov)
   u <- unitSample
   --eval
   let pi = pdf xi
   let pstar = pdf xstar
   let xaccept = if u < exp (pstar - pi)
                    then {-trace ("ACCEPT "++show xstar) -} xstar
                    else {- trace ("REJECT "++show xstar) -} xi   
   if freeze 
      then return $ AMPar xaccept mn cov (round $ t+1)
      else let nmn = L.scale (recip $ t+1) $ xaccept + L.scale t mn
               m0 = L.scale t (L.outer mn mn) - L.scale (t+1) (L.outer nmn nmn) + L.outer xaccept xaccept
               ncov = L.scale ((t-1)/t) cov + L.scale (scalar/t) m0
           in return $ AMPar xaccept nmn ncov (round $ t+1)
--   return $ AMPar xaccept mn cov (round $ t+1)




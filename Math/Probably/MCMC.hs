{-# LANGUAGE Arrows #-}

module Math.Probably.MCMC where

import Math.Probably.Sampler 
import qualified Math.Probably.PDF as P
import Math.Probably.StochFun
import Control.Arrow
import Data.List
import Data.Maybe
import Data.Ord
import Math.Probably.FoldingStats
import TNUtils
import Control.Monad
import Debug.Trace
import Data.Binary

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
                       initial :: !a,
                       unP :: !a } 
               | NonInitialisedParam deriving Show

instance Binary a => Binary (Param a) where
    put (Param j t tt cLH curW ini x) = 
      put (j,t, tt, cLH, curW, ini, x)
    get = do
      (j,t, tt, cLH, curW, ini, x) <- get
      return $ Param j t tt cLH curW ini x


newParam :: a -> Param a
newParam x = Param 0 0 0 (1/0) 1 x x

condDepSampler :: (Double -> b-> b->Sampler a) -> StochFun (Double,b,b) a
condDepSampler dqSam = SF $ \((w,ini,x),dbls) -> unSam (dqSam w ini x) dbls

metSample1P :: Show a => String -> (Double -> a -> a -> Sampler a) -> P.PDF a -> (Param a) -> Sampler (Param a)
metSample1P st prop pdf = uncondSampler $ metropolisLnP st prop pdf

metSample1PCL :: Show a => String -> (Double -> a -> a -> Sampler a) -> P.PDF a -> P.PDF a-> (Param a) -> Sampler (Param a)
metSample1PCL st prop lh prior = uncondSampler $ metropolisLnPCL st prop lh prior


metropolisLnP ::  Show a => String -> (Double -> a-> a-> Sampler a) ->  P.PDF a -> StochFun (Param a) (Param a)
metropolisLnP st qSam p
    = let accept xi pi pstar | notNanInf2 pi pstar =  min 1 $ exp (pstar - pi)
                             | otherwise = cond [(nanOrInf pi && nanOrInf pstar, 
                                                        error $ "metropolisLnP "++st++" pi pi pstar :"++
                                                                show (pi,pstar)++"\n"++
                                                                show xi),
                                                 (nanOrInf pstar, -1), -- never accept
                                                 (nanOrInf pi, 2)] $ error "metropolisLn: the impossible happend"
      in proc par@(Param j t tt _ curw ini xi) -> do
        let (nextw, nj, nt) = calcNextW curw j t tt
        u <- sampler unitSample -< ()
        xstar <- condDepSampler qSam -< (nextw, ini, xi)
        let pstar = p xstar 
        let pi = p xi 
        returnA -< if u < accept par pi pstar
                      then Param (nj+1) (nt+1) (tt+1) pstar nextw ini xstar
                      else Param nj (nt+1) (tt+1) pi nextw ini xi

x `divides` y = y `mod` x == 0

calcNextW w j t tt | mutFreq `divides` t= 
                     let jf = realToFrac j / realToFrac t 
                         nxtW = w*nextW jf in                    
                    {-trace (show (w, nxtW, j, t)) -} (nxtW, 0, 0)
                   | otherwise = (w, j, t)
    where mutFreq = cond [(t<20000, 2000),
                          (t<200000, 5000)] 5000000
                       

nextW jf | jf > 0.80 = 3
         | jf > 0.50 = 1.4
         | jf < 0.05 = 0.3
         | jf < 0.10 = 0.6
         | jf < 0.20 = 0.8
         | otherwise = 1

metropolisLnPCL :: Show a => String -> (Double -> a -> a -> Sampler a) -> P.PDF a -> P.PDF a -> StochFun (Param a) (Param a)
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

manyLike :: (theta -> a -> P.PDF b) -> ([(a,b)] -> P.PDF theta)
manyLike lh1 = \xys -> \theta -> product $ map (\(x,y) -> lh1 theta x y) xys

times :: Monad m => Int -> m a -> m [a]
times n ma = forM [1..n] $ const ma

test = 
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
        print $ regressF `runStat`  zip xs ys



--bayes :: P.PDF a -> P.PDF a -> StochFun a a
--bayes prior likelihood = let numerator = prior `mulPdf` likelihood


--sampler $ uniform 0 1

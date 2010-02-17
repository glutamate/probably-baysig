{-# LANGUAGE CPP, Arrows, ExistentialQuantification, Rank2Types #-}

module Math.Probably.StochFun where

import System.Random.Mersenne
#if __GLASGOW_HASKELL__ > 609
import qualified Control.Category as C
#endif
import Control.Arrow

import Math.Probably.Sampler
import Math.Probably.FoldingStats
import Control.Applicative

import Numeric.FAD

newtype StochFun b c = SF { unSF :: (b, [Double]) -> (c,[Double]) }

sampler :: Sampler a -> StochFun b a
sampler (Sam sf) = SF $ \(_, dbls) -> sf dbls

condSampler :: (b->Sampler a) -> StochFun b a
condSampler sflam = SF $ \(x, dbls) -> (unSam (sflam x)) dbls

uncondSampler :: StochFun b a -> (b->Sampler a) 
uncondSampler (SF sf) = \x -> Sam $ \dbls-> sf (x,dbls)


#if __GLASGOW_HASKELL__ > 609
instance C.Category StochFun where
    id = SF id
    (SF sf1) . (SF sf2) = SF $ \(x,dbls) -> sf1 . sf2 $ (x,dbls)
instance Arrow StochFun where
    arr f = SF $ \(x,dbls) -> (f x, dbls)
    first (SF sf) = SF $ \((x,y),dbls) -> let (x',dbls') = sf (x,dbls) in
                                          ((x',y),dbls')
#else
instance Arrow StochFun where
    arr f = SF $ \(x,dbls) -> (f x, dbls)
    first (SF sf) = SF $ \((x,y),dbls) -> let (x',dbls') = sf (x,dbls) in
                                          ((x',y),dbls')
    (SF sf1) >>> (SF sf2) = SF $ \(x,dbls) -> sf2 . sf1 $ (x,dbls)

#endif 

withCount :: StochFun a a -> StochFun (a,Int) (a,Int)
withCount (SF sf) = SF $ \((x,n),dbls) -> let (x',dbls') = sf (x,dbls) in
                                          ((x',n+1),dbls')

data Markov b = forall a. Mrkv (StochFun a a) a (a->b) 

instance Functor Markov where
    fmap f (Mrkv sf x c) = Mrkv sf x (f . c)

bothMv :: Markov a -> Markov b -> Markov (a,b)
bothMv (Mrkv (SF sf1) x1 c1) (Mrkv (SF sf2) x2 c2) = 
    Mrkv sf (x1,x2) (\(x,y)-> (c1 x, c2 y))
        where sf = SF $ \((x,y),dbls) -> let (x',dbls') = sf1 (x,dbls)
                                             (y', dbls'') = sf2 (y,dbls')
                                         in ((x',y'), dbls'')

instance Applicative Markov where
    pure x = Mrkv (SF id) () $ const x 
    m1 <*> m2 = uncurry ($) <$> bothMv m1 m2

mvSampler :: Sampler a -> Markov a
mvSampler sam = Mrkv (sampler sam) undefined id

runMarkov ::  [Double] -> Markov a -> [a]
runMarkov dbls m@(Mrkv (SF sf) x c) = map c $ run dbls sf x
    where run dbs f y = let (y', dbs') = f (y, dbs)
                         in y' : run dbs' f y'

runMarkovIO :: Markov a -> IO [a]
runMarkovIO m@(Mrkv (SF sf) x c)  = do 
  rnds <- randoms =<< getStdGen 
  return $ runMarkov rnds m


--progressively better means
means :: Fractional a => [a] -> [a]
means = progressively meanF
--means xs = map (\(sm,ln)-> sm/(realToFrac ln)) $ withCount xs


--withCount xs = scanl (\(sm, len) x ->(sm+x, len+1)) (0,(0::Int)) xs




fit :: (Ord a, Floating a) => (forall tag. b -> [Dual tag a] -> Dual tag a) -> [(b,a)] -> [a] -> [[a]]
fit g pts p0 = let ss args = sum $ map (\(t,y)-> (g t args - lift y)**2) pts
               in argminNaiveGradient ss p0

--expDecay :: (Floating a) => a -> [a] -> a
expDecay1 [t, a, tau, s0]  = a*exp(-t*tau)+s0
--expDecay :: (Fractional a, Ord a, Floating a) =>  Double -> (forall tag. [Dual tag a] -> Dual tag a)
expDecay :: (Floating b, Real a) => a -> [b] -> b
expDecay t [a, tau, s0]  = a*exp(-(realToFrac t)/tau)+s0
gaussFun :: (Floating b, Real a) => a -> [b] -> b
gaussFun x [mean, sd] = let factor = (recip $ sd*sqrt (2*pi))
                        in factor * (exp . negate $ ((((realToFrac x)-mean)**2)/(2*sd**2)))
gaussFunOff :: (Floating b, Real a) => a -> [b] -> b
gaussFunOff x [mean, sd, amp, offset] = let factor = (recip $ sd*sqrt (2*pi))
                                        in amp* factor * (exp . negate $ ((((realToFrac x)-mean)**2)/(2*sd**2))) + offset

cauchyFun :: (Floating b, Real a) => a -> [b] -> b
cauchyFun x [x0, gamma] = let diff = realToFrac x - x0
                          in (gamma/pi) * recip (diff*diff+gamma*gamma)

fitFun :: [a] -> (b -> [a] -> a) -> (b->a)

fitFun pars f = \t->f t pars 

--pars= (!!201) $ fit expDecay (pts) [100, 2, 20] in
--            FunSeg 0 30 $ fitFun pars expDecay

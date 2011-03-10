{- |

Stochastic functions, that is sampling functions that take an
argument. These form an arrow and are the basis of Markov Chains.

-}

{-# LANGUAGE CPP, Arrows, ExistentialQuantification, Rank2Types #-}

module Math.Probably.StochFun where

#if __GLASGOW_HASKELL__ > 609
import qualified Control.Category as C
#endif
import Control.Arrow

import Math.Probably.Sampler
import Math.Probably.FoldingStats
import Control.Applicative

newtype StochFun b c = SF { unSF :: (b, Seed) -> (c,Seed) }

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

runMarkov ::  Seed -> Markov a -> [a]
runMarkov dbls m@(Mrkv (SF sf) x c) = map c $ run dbls sf x
    where run dbs f y = let (y', dbs') = f (y, dbs)
                         in y' : run dbs' f y'

runMarkovFor ::  Int -> Seed -> Markov a -> (a, Seed)
runMarkovFor n dbls m@(Mrkv (SF sf) x c) = onFst c $ run n dbls sf x
    where run 0 seed f y = (y, seed)
          run n dbs f y = let (y', dbs') = f (y, dbs)
                          in run (n-1) dbs' f y'
          onFst f (x,y) = (f x, y)


runMarkovIO :: Markov a -> IO [a]
runMarkovIO m@(Mrkv (SF sf) x c)  = do 
  rnds <- getSeedIO 
  return $ runMarkov rnds m

--progressively better means
means :: Fractional a => [a] -> [a]
means = progressively meanF


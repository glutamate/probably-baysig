{-# LANGUAGE Arrows, ExistentialQuantification #-}

module Math.Probably.StochFun where

import System.Random.Mersenne
import qualified Control.Category as C
import Control.Arrow

import Math.Probably.Sampler
import Math.Probably.FoldingStats
import Control.Applicative

newtype StochFun b c = SF { unSF :: (b, [Double]) -> (c,[Double]) }

sampler :: Sampler a -> StochFun b a
sampler (Sam sf) = SF $ \(_, dbls) -> sf dbls

instance C.Category StochFun where
    id = SF id
    (SF sf1) . (SF sf2) = SF $ \(x,dbls) -> sf1 . sf2 $ (x,dbls)

instance Arrow StochFun where
    arr f = SF $ \(x,dbls) -> (f x, dbls)
    first (SF sf) = SF $ \((x,y),dbls) -> let (x',dbls') = sf (x,dbls) in
                                          ((x',y),dbls)

withCount :: StochFun a a -> StochFun (a,Int) (a,Int)
withCount (SF sf) = SF $ \((x,n),dbls) -> let (x',dbls') = sf (x,dbls) in
                                          ((x',n+1),dbls)



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
    pure x = Mrkv C.id () $ const x 
    m1 <*> m2 = uncurry ($) <$> bothMv m1 m2

mvSampler :: Sampler a -> Markov a
mvSampler sam = Mrkv (sampler sam) undefined id

runMarkov ::  [Double] -> Markov a -> [a]
runMarkov dbls m@(Mrkv (SF sf) x c) = let (x', dbls') = sf (x,dbls)
                                      in c x:runMarkov dbls' m

runMarkovIO :: Markov a -> IO [a]
runMarkovIO m@(Mrkv (SF sf) x c)  = do 
  rnds <- randoms =<< getStdGen 
  return $ runMarkov rnds m


--progressively better means
means :: Fractional a => [a] -> [a]
means = progressively meanF
--means xs = map (\(sm,ln)-> sm/(realToFrac ln)) $ withCount xs


--withCount xs = scanl (\(sm, len) x ->(sm+x, len+1)) (0,(0::Int)) xs
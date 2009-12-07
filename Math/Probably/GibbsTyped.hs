{-# LANGUAGE GADTs, TypeOperators, ScopedTypeVariables #-}
module Math.Probably.Gibbs where

import Math.Probably.Sampler
import Math.Probably.StochFun

data a :+: b = a :+: b deriving (Eq, Show)
data TNil = TNil  deriving (Eq, Show)

infixr 3 :+:
infixr 2 :%:

data GTerm all b where
    GNil :: GTerm a TNil
    (:%:) :: (a -> Sampler b) -> GTerm a c -> GTerm a (b :+: c)

type Gibbs a = GTerm a a

myG2 :: Gibbs (Double :+: Double :+: TNil)
myG2 =  (\(x:+:y:+:_)-> return 0.5) :%: ((\(x:+:y:+:_)->unitSample) :%: GNil)


gibbsSampler :: GTerm a b -> StochFun a b
gibbsSampler gibbs = SF stochfun
    where stochfun (x0, dbls) = undefined

--there is a serious flaw in this approach. we need to replace items
--in x0 when running subsequent samplers. this is considerably more
--difficult.
nearlyGibbs ::  GTerm a b -> StochFun a b
nearlyGibbs (f :%: g) = SF $ \(x0, dbls) -> let Sam sf = f x0
                                                (y, dbls') = sf dbls
                                                next = unSF $ nearlyGibbs g
                                                (rest, dbls'') = next (x0, dbls')
                                             in (y :+: rest, dbls'')
nearlyGibbs GNil = sampler $ return TNil


myChain = Mrkv (nearlyGibbs myG2) (2:+:2:+:TNil) id

tst = take 20 `fmap` runMarkovIO myChain


{-iterateG :: GTerm a b -> a -> b
iterateG (f :%: g) x0 = f x0 :+: iterateG g x0
iterateG GNil _ = TNil

myXs :: [Double :+: (Double :+: TNil)]
myXs = iterate (iterateG myG2) (2 :+: (3 :+: TNil))
-}

gibbs2 :: (a-> b) -> (b-> a) -> (a,b) -> (a,b)
gibbs2 f g (x,y) = let y1 = f x
                       x1 = g y1 in (x1,y1)

gibbs3 :: ((a, b)->c) -> ((b,c)-> a) -> ((c,a)-> b) -> (a,b,c) -> (a,b,c)
gibbs3 f g h (x0,y0,z0) = let x1 = g (y0,z0)
                              y1 = h (z0, x1)
                              z1 = f (x1, y1)
                          in (x1,y1,z1)


asD :: Double -> Double
asD = id

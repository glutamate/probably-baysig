{-# LANGUAGE GADTs, TypeOperators, ScopedTypeVariables #-}
module Math.Probably.Gibbs where

import Math.Probably.Sampler

data a :+: b = a :+: b deriving (Eq, Show)
data TNil = TNil  deriving (Eq, Show)


data GTerm all b where
    GNil :: GTerm a TNil
    (:%:) :: (a ->  b) -> GTerm a c -> GTerm a (b :+: c)

type Gibbs a = GTerm a a

myG2 :: Gibbs (Double :+: (Double :+: TNil))
myG2 =  (\(x:+:(y:+:_))-> 2*x+1) :%: ((\(x:+:(y:+:_))->2*y-3) :%: GNil)


iterateG :: GTerm a b -> a -> b
iterateG (f :%: g) x0 = f x0 :+: iterateG g x0
iterateG GNil _ = TNil

myXs :: [Double :+: (Double :+: TNil)]
myXs = iterate (iterateG myG2) (2 :+: (3 :+: TNil))


gibbs2 :: (a-> b) -> (b-> a) -> (a,b) -> (a,b)
gibbs2 f g (x,y) = let y1 = f x
                       x1 = g y1 in (x1,y1)


asD :: Double -> Double
asD = id
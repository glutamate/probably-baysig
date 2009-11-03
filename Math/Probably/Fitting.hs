{-# LANGUAGE BangPatterns, Rank2Types #-}

module Math.Probably.Fitting where

import Numeric.FAD


fit :: (Ord a, Floating a) => (forall tag. b -> [Dual tag a] -> Dual tag a) -> [(b,a)] -> [a] -> [[a]]
fit g pts p0 = let ss args = sum $ map (\(t,y)-> (g t args - lift y)**2) pts
               in argminNaiveGradient ss p0

--expDecay :: (Floating a) => a -> [a] -> a
expDecay1 [t, a, tau, s0]  = a*exp(-t*tau)+s0
--expDecay :: (Fractional a, Ord a, Floating a) =>  Double -> (forall tag. [Dual tag a] -> Dual tag a)
expDecay :: (Floating b, Real a) => a -> [b] -> b
expDecay t [a, tau, s0]  = a*exp(-(realToFrac t)/tau)+s0

gaussFun x [mean, sd] = let factor = (recip $ sd*sqrt (2*pi))
                        in factor * (exp . negate $ (((x-mean)**2)/(2*sd**2)))

fitFun :: [a] -> (b -> [a] -> a) -> (b->a)
fitFun pars f = \t->f t pars 

--pars= (!!201) $ fit expDecay (pts) [100, 2, 20] in
--            FunSeg 0 30 $ fitFun pars expDecay
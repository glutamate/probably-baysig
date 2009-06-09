module SamFun where

import System.Random
import Control.Monad

newtype SamFun a = SF {unSF :: [Double] -> (a, [Double]) }

unitSample :: SamFun Double
unitSample = SF $ \(r:rs) -> (r,rs)

instance Functor SamFun where
    fmap f (SF sf) = SF $ \rs -> let (x,rs') = sf rs in
                                 (f x, rs')

instance Monad SamFun where
    return x = SF (\rs-> (x, rs))
    (SF sf) >>= f = SF $ \rs-> let (x, rs') = sf rs in
                               (unSF $ f x) rs'

joint :: SamFun a -> SamFun b -> SamFun (a,b)
joint sf1 sf2 = liftM2 (,) sf1 sf2

uniform a b = (\x->x*(b-a)+a) `fmap` unitSample
                
--http://en.wikipedia.org/wiki/Box-Muller_transform
gauss m sd = 
    do (u1,u2) <- joint unitSample unitSample
       return $ sqrt(-2*log(u1))*cos(2*pi*u2)*sd+m


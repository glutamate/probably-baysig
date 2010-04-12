{-# LANGUAGE ViewPatterns, NoMonomorphismRestriction, FlexibleInstances #-}
module Math.Probably.PDF where

import Math.Probably.Student

type PDF a = a->Double

uniform :: (Real a, Fractional a) => a-> a-> PDF a
uniform from to = \x-> if x>=from && x <=to
                               then realToFrac $ 1/(to-from )
                               else 0

--http://en.wikipedia.org/wiki/Normal_distribution
gauss :: (Real a, Floating a) => a-> a-> PDF a
gauss mean sd = \x->realToFrac $ recip(sd*sqrt(2*pi))*exp(-(x-mean)**2/(2*sd*sd))

gaussD :: Double -> Double-> PDF Double
gaussD mean sd = \x->recip(sd*sqrt(2*pi))*exp(-(x-mean)**2/(2*sd*sd))

logGaussD :: Double -> Double-> PDF Double
logGaussD mean sd x = negate $ (x-mean)**2/(2*sd*sd) + log(sd*sqrt(2*pi))

gammafun = exp . gammaln

--http://en.wikipedia.org/wiki/Gamma_distribution
gammaD :: Double -> Double -> PDF Double
gammaD k theta x = x**(k-1)*(exp(-x/theta)/(theta**k*(exp (gammaln k))))

invGammaD :: Double -> Double -> PDF Double
invGammaD a b x =(b**a/gammafun a)*(1/x)**(a+1)*exp(-b/x)

logNormal m sd x = recip (x*sd*sqrt(2*pi)) * exp (negate $ square (log x - m) / (2*square sd))
    where square x = x*x

logLogNormal m sd x = negate $ (x*sd*sqrt(2*pi)) + square (log x - m) / (2*square sd)
    where square x = x*x


{-# SPECIALIZE gauss :: Double -> Double-> PDF Double #-}

--this is a Prob. Mass function, not density.
binomial :: Int -> Double -> PDF Int
binomial n p k = let realk = realToFrac k
                 in realToFrac (choose  n k) * p**realk * (1-p)**(realToFrac $ n-k)

zipPdfs :: Num a => PDF a -> PDF b -> PDF (a,b)
zipPdfs fx fy (x,y) = fx x * fy y

mulPdf :: Num a => PDF a -> PDF a -> PDF a
mulPdf d1 d2 = \x -> (d1 x * d2 x)

--instance Num a => Num (PDF a) where
    
--http://blog.plover.com/math/choose.html
choose n 0 = 1
choose 0 k = 0
choose (n+1) (k+1) = (choose n k) * (n+1) `div` (k+1)

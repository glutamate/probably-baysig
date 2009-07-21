module Math.Probably.PDF where

type PDF a = a->Double

uniform :: (Real a, Fractional a) => a-> a-> PDF a
uniform from to = \x-> if x>=from && x <=to
                               then realToFrac $ 1/(to-from )
                               else 0

--http://en.wikipedia.org/wiki/Normal_distribution
gauss :: (Real a, Floating a) => a-> a-> PDF a
gauss mean sd = \x->realToFrac $ recip(sd*sqrt(2*pi))*exp(-(x-mean)**2/(2*sd*sd))
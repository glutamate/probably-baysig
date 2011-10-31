module Math.Probably.Student where

import Math.Probably.FoldingStats
import Numeric.LinearAlgebra
--http://www.haskell.org/haskellwiki/Gamma_and_Beta_function
cof :: [Double]
cof = [76.18009172947146,-86.50532032941677,24.01409824083091,-1.231739572450155,0.001208650973866179,-0.000005395239384953]
 
ser :: Double
ser = 1.000000000190015
 
gammaln :: Double -> Double
gammaln xx = let tmp' = (xx+5.5) - (xx+0.5)*log(xx+5.5)
                 ser' = foldl (+) ser $ map (\(y,c) -> c/(xx+y)) $ zip [1..] cof
             in -tmp' + log(2.5066282746310005 * ser' / xx)

beta z w = exp (gammaln z + gammaln w - gammaln (z+w))

fac n = product [1..n]

ixbeta x a b = let top = fac $ a+b-1                        
                   down j = fac j * fac (a+b-1-j)                   
               in sum $ map (\j->(top/down j)*(x**j)*(1-x)**(a+b-1-j)) [a..a+b-1]

studentIntegral t v = 1-ixbeta (v/(v+t*t)) (v/2) (1/2)

oneSampleT v0 = fmap (\(mean,sd,n)-> (mean - v0)/(sd/(sqrt n))) meanSDNF 

pairedSampleT  = before (fmap (\(mean,sd,n)-> (mean)/(sd/(sqrt n))) meanSDNF)
                        (uncurry (-))

tTerms = fromList $ map tTermUnmemo [1..100]

tTermUnmemo nu = gammaln ((realToFrac nu+1)/2) - log(realToFrac nu*pi)/2 - gammaln (realToFrac nu/2)

tTerm1 :: Int -> Double
tTerm1 df | df <= 100 = tTerms@>df
          | otherwise = tTermUnmemo df

tDist df t = tTerm1 df - (realToFrac df +1/2) * log (1+(t*t)/(realToFrac df))

tDist3 mean prec df x 
    =   tTerm1 df 
      + log(prec)/2 
      - (realToFrac df +1/2) * log (1+(prec*xMinusMu*xMinusMu)/(realToFrac df))
 where xMinusMu = x-mean
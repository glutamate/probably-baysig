module Math.Probably.Student where

import Numeric.LinearAlgebra
import Math.Probably.FoldingStats
import Text.Printf

--http://www.haskell.org/haskellwiki/Gamma_and_Beta_function
--cof :: [Double]
cof = [76.18009172947146,-86.50532032941677,24.01409824083091,
       -1.231739572450155,0.001208650973866179,-0.000005395239384953]
 
--ser :: Double
ser = 1.000000000190015
 
--gammaln :: Double -> Double
gammaln xx = let tmp' = (xx+5.5) - (xx+0.5)*log(xx+5.5)
                 ser' = foldl (+) ser $ map (\(y,c) -> c/(xx+y)) $ zip [1..] cof
             in -tmp' + log(2.5066282746310005 * ser' / xx)

beta z w = exp (gammaln z + gammaln w - gammaln (z+w))

fac :: (Enum a, Num a) => a -> a
fac n = product [1..n]

ixbeta :: (Enum a, Floating a) => a -> a -> a -> a
ixbeta x a b = let top = fac $ a+b-1                        
                   down j = fac j * fac (a+b-1-j)                   
               in sum $ map (\j->(top/down j)*(x**j)*(1-x)**(a+b-1-j)) [a..a+b-1]

studentIntegral :: (Enum a, Floating a) => a -> a -> a
studentIntegral t v = 1-ixbeta (v/(v+t*t)) (v/2) (1/2)


tTerms :: Vector Double
tTerms = fromList $ map tTermUnmemo [1..100]

tTermUnmemo :: Int -> Double
tTermUnmemo nu = gammaln ((realToFrac nu+1)/2) - log(realToFrac nu*pi)/2 - gammaln (realToFrac nu/2)

tTerm1 :: Int -> Double
tTerm1 df | df <= 100 = tTerms@>df
          | otherwise = tTermUnmemo df

tDist :: Int -> Double -> Double
tDist df t = tTerm1 df - (realToFrac df +1/2) * log (1+(t*t)/(realToFrac df))

tDist3 :: Double -> Double -> Int -> Double -> Double
tDist3 mean prec df x 
    =   tTerm1 df 
      + log(prec)/2 
      - (realToFrac df +1/2) * log (1+(prec*xMinusMu*xMinusMu)/(realToFrac df))
 where xMinusMu = x-mean

oneSampleT :: Floating b => b -> Fold b b
oneSampleT v0 = fmap (\(mean,sd,n)-> (mean - v0)/(sd/(sqrt n))) meanSDNF 

pairedSampleT :: Fold (Double, Double) Double
pairedSampleT  = before (fmap (\(mean,sd,n)-> (mean)/(sd/(sqrt n))) meanSDNF)
                        (uncurry (-))

data TTestResult = TTestResult { degFreedom :: Int,
                                 tValue :: Double,
                                 pValue :: Double }

ppTTestRes :: TTestResult -> String
ppTTestRes (TTestResult df tval pval) = printf "paired t(%d)=%.3g, p=%.3g" df tval pval

pairedTTest :: [(Double,Double)] -> TTestResult
pairedTTest vls =
    let tval = runStat pairedSampleT vls
        df =  length vls - 1
        pval =  (1-) $ studentIntegral (tval) (realToFrac df)
    in TTestResult df tval pval

oneSampleTTest :: [Double] -> TTestResult
oneSampleTTest vls =
  let tval = runStat (oneSampleT 0) vls
      df =  length vls - 1
      pval =  (1-) $ studentIntegral (tval) (realToFrac df)
  in TTestResult df tval pval

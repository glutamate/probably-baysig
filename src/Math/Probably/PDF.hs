-- |
-- Defines Log-domain probability density functions


{-# LANGUAGE ViewPatterns, NoMonomorphismRestriction, FlexibleInstances #-}
module Math.Probably.PDF where

import qualified Math.Probably.Student as S
import Numeric.LinearAlgebra
import Control.Spoon
import Control.DeepSeq
import qualified Data.Vector.Storable as VS

-- | The type of probablility density functions
type PDF a = a->Double

-- | Uniform distributions
uniform :: (Real a, Fractional a) => a-> a-> PDF a
uniform from to = \x-> if x>=from && x <=to
                               then log $ realToFrac $ 1/(to-from )
                               else 0

--http://en.wikipedia.org/wiki/Normal_distribution

-- | Normal distribution by variance
normal :: (Real a, Floating a) => a-> a-> a -> a
normal = normalLogPdf

normalLogPdf :: (Real a, Floating a) => a-> a-> a -> a
normalLogPdf mean variance x
   = ((log 1)-(0.500*(log ((2.000*pi)*variance))))-(((x-mean)**2)/(2*variance))

normalSdPdf mean sd x = exp(-(x-mean)^2/(2*sd^2))/(sd*sqrt(2*pi))

normalSdLogPdf mean sd x = -(x-mean)^2/(2*sd^2) - log(sd) -log(2*pi)/2

dNormSd_wrt_mean mean sd x = (x-mean)/(sd*sd)

dNormSd_wrt_sd m s x = ((m-x)^2-s*s)/(s*s*s)

dNormSd_wrt_draw m s x = (m-x)/(s*s)

-- | Normal distribution, specialised for Doubles
gammafun = exp . S.gammaln

-- | <http://en.wikipedia.org/wiki/Gamma_distribution>
gamma :: Double -> Double -> PDF Double
gamma k theta x = log $ x**(k-1)*(exp(-x/theta)/(theta**k*(exp (S.gammaln k))))

polygamma :: (Real a, Floating a, Enum a) => a -> a -> a-> a
polygamma k theta x = log $ x**(k-1)*(exp(-x/theta)/(theta**k*(exp (polygammaln k))))

polycof = [76.18009172947146,-86.50532032941677,24.01409824083091,
       -1.231739572450155,0.001208650973866179,-0.000005395239384953]

--ser :: Double
polyser = 1.000000000190015

--gammaln :: Double -> Double
polygammaln xx = let tmp' = (xx+5.5) - (xx+0.5)*log(xx+5.5)
                     ser' = foldl (+) polyser $ map (\(y,c) -> c/(xx+y)) $ zip [1..] polycof
                 in -tmp' + log(2.5066282746310005 * ser' / xx)


invGamma :: Double -> Double -> PDF Double
invGamma a b x =log $ (b**a/gammafun a)*(1/x)**(a+1)*exp(-b/x)


logNormal :: Double -> Double-> PDF Double
logNormal = logNormal

-- | Beta distribution
beta a b x = log $ (recip $ S.beta a b) * x ** (a-1) * (1-x) ** (b-1)

-- | Binomial distribution. This is the Probability Mass Function, not PDF
binomial :: Int -> Double -> PDF Int
binomial n p k =
    let realk = realToFrac k
    in log (realToFrac (choose (toInteger n) (toInteger k))) + realk * log( p ) + (realToFrac $ n-k) * log( (1-p) )
 where --http://blog.plover.com/math/choose.html
   choose :: Integer -> Integer -> Integer
   choose n 0 = 1
   choose 0 k = 0
   choose (n) (k) |  k > n `div` 2 = choose n (n-k)
                  |  otherwise = (choose (n-1) (k-1)) * (n) `div` (k)

-- | the joint distribution of two independent distributions
zipPdfs :: Num a => PDF a -> PDF b -> PDF (a,b)
zipPdfs fx fy (x,y) = fx x + fy y

-- | the multiplication of two PDFS
mulPdf :: Num a => PDF a -> PDF a -> PDF a
mulPdf d1 d2 = \x -> (d1 x + d2 x)

--instance Num a => Num (PDF a) where

instance NFData (Matrix Double)
   where rnf mat = mapMatrix (\x-> x `seq` 1.0::Double) mat `seq` ()

-- | multivariate normal
multiNormal :: Vector Double -> Matrix Double -> PDF (Vector Double)
multiNormal mu sigma =
  let k = realToFrac $ dim mu
      invSigma =  inv sigma
      mat1 = head . head . toLists
  in \x-> log (recip ((2*pi)**(k/2) * sqrt(det sigma))) + (mat1 $ negate $ 0.5*(asRow $ x-mu) `multiply` invSigma `multiply` (asColumn $ x-mu) )


multiNormalByInv :: Double -> Matrix Double -> Vector Double -> PDF (Vector Double)
multiNormalByInv lndet invSigma mu =
  let k = realToFrac $ dim mu
      mat1 = head . head . toLists
  in \x-> log 1 - (k/2)*log (2*pi) - lndet/2 + (mat1 $ negate $ 0.5*(asRow $ x-mu) `multiply` invSigma `multiply` (asColumn $ x-mu) )

multiNormalByInvFixCov ::  Matrix Double -> Vector Double -> PDF (Vector Double)
multiNormalByInvFixCov invSigma mu =
  let k = realToFrac $ dim mu
      mat1 = head . head . toLists
  in \x-> (mat1 $ negate $ 0.5*(asRow $ x-mu) `multiply` invSigma `multiply` (asColumn $ x-mu) )


multiNormalIndep :: Vector Double -> Vector Double -> PDF (Vector Double)
multiNormalIndep vars mus xs= VS.sum $ VS.zipWith3 (\var mu x -> normalLogPdf mu var x) vars mus xs


{-mu1 = 2 |> [0, 0::Double]
sig1 = (2><2)[1::Double, 0,
              0, 1]

tst = multiNormal mu1 sig1 mu1
tsta = inv sig1
tstb = det sig1
tstc = let mu = mu1
           sigma = sig1
           x = mu1
           invSigma = inv sigma
       in (asRow $ x-mu) `multiply` invSigma `multiply` (asColumn $ x-mu)  -}


posdefify m =
   let (eigvals, eigvecM) = eigSH $ mkSym {- $ trace (show m) -}  m
       n = rows m
       eigValsVecs = map f $ zip (toList eigvals) (toColumns eigvecM)
       f (val,vec) = (abs val,vec)
       q = fromColumns $ map snd eigValsVecs
       bigLambda = diag $ fromList $ map fst eigValsVecs
   in mkSym $ q `multiply` bigLambda `multiply` inv q

mkSym m = buildMatrix (rows m) (cols m)$ \(i,j) ->if i>=j then m @@>(i,j)
                                                           else m @@>(j,i)

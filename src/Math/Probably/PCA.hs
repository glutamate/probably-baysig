{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}

module Math.Probably.PCA where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.HMatrix ((#>))
import Math.Probably.FoldingStats
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as VS
import Foreign.Storable.Tuple ()
import Control.Spoon
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import           Data.List
import           Data.Ord
import           Data.Number.Erf
import Debug.Trace


empiricalCovariance :: [Vector Double] -> Matrix Double
empiricalCovariance xs
   = let k = length xs
         --barxk = L.scale (recip $ realToFrac $ k+1) $ sum xs
         --m1 = sum $ map (\xv-> L.outer xv xv) xs
         --m2 = L.scale (realToFrac $ k+1) $ L.outer barxk barxk
         xmn = empiricalMean xs
         f xi = outerSame $ xi - xmn
--     in L.scale (recip $ realToFrac k) $ m1 - m2
     in scale (recip $ realToFrac k - 1 ) $ sum $ map f xs

empiricalCovariance' :: Vector Double -> [Vector Double] -> Matrix Double
empiricalCovariance' xmn xs
   = let k = length xs
         --barxk = L.scale (recip $ realToFrac $ k+1) $ sum xs
         --m1 = sum $ map (\xv-> L.outer xv xv) xs
         --m2 = L.scale (realToFrac $ k+1) $ L.outer barxk barxk
         f xi = outerSame $ xi - xmn
--     in L.scale (recip $ realToFrac k) $ m1 - m2
     in scale (recip $ realToFrac k - 1 ) $ sumWith f xs


outerSame v = outer v v

--sumWithVs :: (Vector Double -> Vector Double) -> [Vector Double] -> Vector Double
--sumWithVs f = foldl1' (\acc v -> acc + f v)

sumWith :: Num a => (Vector Double -> a) -> [Vector Double] -> a
sumWith f (v:vs) = foldl' (\acc v -> acc + f v) (f v) vs

empiricalMean :: [Vector Double] -> Vector Double
empiricalMean vecs = scale (recip $ realToFrac $ length vecs) $ sumWith id vecs

-- copied from https://github.com/albertoruiz/hmatrix/blob/master/examples/pca2.hs

type Vec = Vector Double
type Mat = Matrix Double

-- Vector with the mean value of the columns of a matrix
mean a = constant (recip . fromIntegral . rows $ a) (rows a) <> a

-- covariance matrix of a list of observations stored as rows
cov x = (trans xc <> xc) / fromIntegral (rows x - 1)
    where xc = x - asRow (mean x)

cov0 x = (trans x <> x) / fromIntegral (rows x - 1)
    --where xc = x - asRow (mean x)


type Stat = (Vec, [Double], Mat)
-- 1st and 2nd order statistics of a dataset (mean, eigenvalues and eigenvectors of cov)
stat :: Mat -> Stat
stat x = (m, toList s, trans v) where
    m = mean x
    (s,v) = eigSH' (cov x)

stat0 :: Mat -> Stat
stat0 x = (m, toList s, trans v) where
    m = VS.map (const 0) s
    c = cov0 x
    (s,v) = eigSH' $ trace ("cov00="++(show (c!0!0))) $ c

statSVD :: Mat -> Stat
statSVD x = (m, [], evecs) where
    m = mean x
    c = cov x
    (_, evals, evecCols) = svd c
    evecs = fromRows $ map evecSigns $ toColumns evecCols
    evecSigns ev = let maxelidx = maxIndex $ cmap abs ev
                       sign = signum (ev ! maxelidx)
                   in cmap (sign *) ev

statThinSVD :: Mat -> Stat
statThinSVD x = (m, [], evecs) where
    m = mean x
    c = cov x
    (_, evals, evecCols) = thinSVD c
    evecs = fromRows $ map evecSigns $ toColumns evecCols
    evecSigns ev = let maxelidx = maxIndex $ cmap abs ev
                       sign = signum (ev ! maxelidx)
                   in cmap (sign *) ev

pcaNSVD :: Int -> Stat -> (Vec -> Vec , Vec -> Vec)
pcaNSVD  n (m,_,evecs) = (encode,decode)
  where
    encode x = VS.take n $ evecs #> (x - m)
    decode x = error "not sure right now"

statVs :: [Vector Double] -> Stat
statVs vs = (m, toList s, trans v) where
    m = empiricalMean vs
    (s,v) = eigSH' (empiricalCovariance' m vs)

mbStat :: Mat -> Maybe Stat
mbStat = spoon . stat

pca :: Double -> Stat -> (Vec -> Vec , Vec -> Vec)
pca prec (m,s,v) = (encode,decode)
  where
    encode x = vp <> (x - m)
    decode x = x <> vp + m
    vp = takeRows n v
    n = 1 + (length $ fst $ span (< (prec'*sum s)) $ cumSum s)
    cumSum = tail . scanl (+) 0.0
    prec' = if prec <=0.0 || prec >= 1.0
                then error "the precision in pca must be 0<prec<1"
                else prec

pcaN :: Int -> Stat -> (Vec -> Vec , Vec -> Vec)
pcaN n (m,s,v) = (encode,decode)
  where
    encode x = vp <> (x - m)
    decode x = x <> vp + m
    vp = takeRows n v

type Lens a b = ( (a->b), (a-> b -> a) )

get :: Lens a b -> a -> b
get (f,_) x = f x

set :: Lens a b -> a -> b -> a
set (_,f) x y = f x y



--, pcaFeatures
normaliseBy :: (Eq b, Ord b) => Lens a Vec -> Lens a b -> Int -> [a] -> [a]
normaliseBy veclens idlens ncomponents = equaliseVars veclens
                                         . rankTransform veclens idlens
                                         . pcaFeatures veclens ncomponents
                                         . equaliseVars veclens

normaliseNoRank :: Lens a Vec -> Int -> [a] -> [a]
normaliseNoRank veclens  ncomponents = equaliseVars veclens
                                         . pcaFeatures veclens ncomponents
                                         . equaliseVars veclens


equaliseVars ::Lens a Vec -> [a] -> [a]
equaliseVars veclens xs = equal_vars where
  decorr_vars :: VS.Vector (Double,Double)
  decorr_vars = uncurry (VS.zipWith (,)) $ runStat meanSDF $ map (get veclens) xs

  equal_vars =  map (\x -> set veclens x (g $ get veclens x)) xs

  g :: Vec -> Vec
  g fl = VS.zipWith (\x (mn,sd) -> (x-mn)/sd) fl decorr_vars

vecMeanSds :: [Vector Double] -> Vector (Double, Double)
vecMeanSds xs = uncurry (VS.zipWith (,)) $ runStat meanSDF  xs

vecMeans :: [Vector Double] -> Vector Double
vecMeans xs = scale (recip $ realToFrac $ length xs) $ sumWith id xs



rankTransform :: (Eq b, Ord b) => Lens a Vec -> Lens a b -> [a] -> [a]
rankTransform veclens idlens nmfeatures = rankTransformed where
  nfeatures = VS.length $ get veclens $ head $ nmfeatures
  nposters = realToFrac $ length nmfeatures
  sortations = flip map [0..(nfeatures-1)] $ \ix -> toRankMap $ map (get idlens) $ sortBy (comparing ((VS.! ix) . get veclens)) nmfeatures -- decorrelated
  rankTransformed = flip map nmfeatures $ \x ->
                      let nm = get idlens x
                          f sortation = invnormcdf $ realToFrac ((getRank sortation nm)+1) / (nposters+1)
                          newv =  VS.fromList $ map f sortations
                      in set veclens x newv

pcaFeatures :: Lens a Vec -> Int -> [a] -> [a]
pcaFeatures veclens ncomp nmfeatures = pca'd where
  input_data = map (get veclens ) nmfeatures
  pcaStats@(_,eigVals,_) = stat $ fromRows input_data
  (enc,_) = trace ("eigenVals ="++show eigVals ) $ pcaN ncomp pcaStats
--  pca_red = PCA.pcaReduceN input_data 5
  setFeatures x =
      let oldv = get veclens x
          newNormV = enc oldv
      in set veclens x newNormV
  pca'd = map setFeatures nmfeatures

type RankMap a = M.Map a Int

toRankMap :: (Ord a, Eq a) => [a] -> RankMap a
toRankMap = M.fromList . (`zip` [0..])

getRank :: (Ord a, Eq a) => RankMap a -> a -> Int
getRank m x = fromJust $ M.lookup x m


newtype NumUV a = NumUV { unNumUV :: U.Vector a } deriving (Eq, Ord)

numUmap f = NumUV . U.map f . unNumUV
numUlength = U.length . unNumUV

instance (U.Unbox a, Num a) => Num (NumUV a) where
    (+) = binvop (+)
    (-) = binvop (-)
    (*) = binvop (*)
    abs = numUmap abs
    signum = numUmap signum
    fromInteger x = NumUV $ U.fromList [fromInteger x]

instance (U.Unbox a, Fractional a) => Fractional (NumUV a) where
    (/) = binvop (/)
    fromRational x = NumUV $ U.fromList [fromRational x]
instance (U.Unbox a, Floating a) => Floating (NumUV a) where
    pi = NumUV $ U.fromList [pi]
    sqrt = numUmap sqrt
    log = numUmap log
    exp = numUmap exp
    sin = numUmap sin
    cos = numUmap cos
    tan = numUmap tan
    asin = numUmap asin
    acos = numUmap acos
    atan = numUmap atan
    sinh = numUmap sinh
    cosh = numUmap cosh
    tanh = numUmap tanh
    asinh = numUmap asinh
    acosh = numUmap acosh
    atanh = numUmap atanh

binvop f xs ys
  | numUlength ys == 1 = let y = (unNumUV ys) U.! 0 in numUmap (\x-> f x y) xs
  | numUlength xs == 1 = let x = (unNumUV xs) U.! 0 in numUmap (\y-> f x y) ys
  | otherwise = NumUV $ U.zipWith f (unNumUV xs) (unNumUV ys)

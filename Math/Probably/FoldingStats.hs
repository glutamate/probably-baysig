{-# LANGUAGE ExistentialQuantification #-}

module Math.Probably.FoldingStats where

--import qualified Data.List as L
import Control.Applicative
import Data.Foldable
import Data.Array.Vector

data Fold b c = forall a. F (a -> b -> a) a (a -> c) (a -> a -> a)
--		| C c
--		| forall d. FMany [Fold b d] ([d] -> c) 

instance Functor (Fold a) where
	fmap = flip after

instance Applicative (Fold a) where
	pure x = F (\_ _ -> ()) () (\_ -> x) (\_ _-> ())
	f <*> g = (uncurry ($)) `fmap` both f g -- :: Fold a (b->c, b) 

{-instance Bounded Double where
    minBound = -1e-200
    maxBound = 1e-200 -}

data P a b = P !a !b

both :: Fold b c -> Fold b c' -> Fold b (c, c')
both (F f x c pc) (F g y c' pc') = F (comb f g) (P x y) (c *** c') 
				     $ \(P a1 a2) (P a1' a2')->P (pc a1 a1') (pc' a2 a2')
    where
        comb f g (P a a') b = P (f a b) (g a' b)
        (***) f g (P x y) = (f x, g y)
--both (F f x c pc) (C v) = F f x (\acc -> (c acc, v)) pc
--both (C v) (F f x c pc) = F f x (\acc -> (v, c acc)) pc
--both (C x) (C y) = C (x,y)

--allOf :: [Fold a b] -> Fold a [b]
--allOf [] = C []
--allOf ((F f x c pc):[])  = F f x (\a->[c a]) pc

after :: Fold b c -> (c -> c') -> Fold b c'
after (F f x c pc) d = F f x (d . c) pc
--after (C v) f = C $ f v

before :: Fold b' c -> (b-> b') -> Fold b c
before (F f x c pc) d = F (\acc nxt -> f acc $ d nxt) x c pc
--before (C v) _ = C v
--The next one, bothWith, is a combination of both and after.


bothWith :: (c -> c' -> d) -> Fold b c -> Fold b c' -> Fold b d
bothWith combiner f1 f2 = after (both f1 f2) (uncurry combiner)

--Now that we have tools to build folds, we want to actually fold them, so here is combinator foldl':


--cfoldl' :: Fold b c -> [b] -> c
--cfoldl' (F f x c _) xs = c $ (L.foldl' f x) xs
--cfoldl' (C v) _ = v

progressively :: Fold b c -> [b] -> [c]
progressively (F f x c _) = map c . (scanl f x) 

runStat :: (Foldable t) => Fold b c -> t b -> c
runStat (F f x c _) = c . (foldl' f x)

runStatU :: (UA b) => Fold b c -> UArr b -> c
runStatU (F f x c _) = c . (foldlU f x) 

runStatOnMany :: Fold b c -> [[b]] -> [c]
runStatOnMany _ [] = []
runStatOnMany (F f x c _) xss = map c $ foldl' f' (replicate n x) xss
    where n  = length xss
          f' = zipWith f

--seqit x = x `seq` x
                     

--runStatWithFold folder (F f x c _) = c . (folder f x) 

--Now lets see a few basic folds:


sumF :: Num a => Fold a a
sumF = F (+) 0 id (+)

sumSqrF :: Num a => Fold a a
sumSqrF = before sumF square

square :: (Num a) => a -> a
square x = x*x

jumpFreqByF :: (a->a->Bool) -> Fold a Double
jumpFreqByF p = F f ini c comb
    where f (Nothing, countj , total) new = (Just new, countj, total)
          f (Just old, countj, total) new | p old new = (Just new, countj, total+1)
                                          | otherwise  = (Just new, countj+1, total+1)
          ini = (Nothing, 0, 0)
          c (_, j, t) = realToFrac j/realToFrac t
          comb (_, a, b) (_, c, d) = (Nothing, a+c, d+b)
                                                         

productF :: Num a => Fold a a
productF = F (*) 1 id (*)

lengthF :: Fold a Int
lengthF = F (const . (+1)) 0 id (+)

realLengthF :: Fractional a => Fold b a
realLengthF = fromIntegral `fmap` lengthF

dotProdF :: Num a => Fold (a,a) a
dotProdF = before sumF $ uncurry (*)

maxF :: (Num a, Ord a, Bounded a) => Fold a a
maxF = F (max) (minBound) id (max)

minF :: (Num a, Ord a, Bounded a) => Fold a a
minF = F (min) (maxBound) id (min)

minLocF :: (Num a, Ord a, Bounded a) => Fold a (a, Int)
minLocF = F (\(minv, minn, curn) v -> if v < minv
                                         then (v,curn, curn+1)
                                         else (minv, minn, curn+1)) (maxBound, -1, 0) (\(x,y,z)->(x,y)) (undefined)

--x = -Infinity
--And, the moment we've all been waiting for, combining basic folds to get the mean of a list:

--sd=((recip len)*sqrt (len*sq-s*s))

--stdDev = pure (/) <*> (sqrt <$> innerDiff) <*> realLengthF
--	where innerDiff = pure (-) <*> (pure (*) <*> realLengthF <*> sumSqrF) <*> (before sumF square)

--varF = (pure (-) <*> sumSqrDivN <*> (square `fmap` meanF))

--varPF = (pure (-) <*> (pure (*) <*> (recip  `fmap` realLengthF) <*> sumSqrF) <*> (square `fmap` meanF))
--http://en.wikipedia.org/wiki/Standard_deviation#Rapid_calculation_methods
--stdDevP1 = f <$> nSumSumSqr 
--    where f ((s0, s1), s2) = (recip s0)*sqrt(s0*s2-s1*s1)

stdDevF, stdDevPF :: Floating a => Fold a a
stdDevF = sqrt <$> varF
stdDevPF = sqrt <$> varPF

varF, varPF :: Fractional a => Fold a a
varF = f <$> nSumSumSqr 
    where f ((s0, s1), s2) = (s0*s2-s1*s1)/(s0*(s0-1))
varPF = f <$> nSumSumSqr 
    where f ((s0, s1), s2) = recip (s0*s0)*(s0*s2-s1*s1)


sumSqrDivN :: Fractional a => Fold a a
sumSqrDivN = pure (*) <*> ((recip . decr) `fmap` realLengthF) <*> sumSqrF

decr x = x-1
-- correct?
meanF :: Fractional a => Fold a a
meanF = pure (/) <*> sumF <*> realLengthF

meanSDF :: Floating a => Fold a (a,a)
meanSDF = f <$> nSumSumSqr 
    where f ((s0, s1), s2) = (s1/s0, sqrt $ (s0*s2-s1*s1)/(s0*(s0-1)))

meanSDNF :: Floating a => Fold a (a,a,a)
meanSDNF = f <$> nSumSumSqr 
    where f ((s0, s1), s2) = (s1/s0, sqrt $ (s0*s2-s1*s1)/(s0*(s0-1)), s0)


meanSEMF :: Floating a => Fold a (a,a)
meanSEMF = f <$> nSumSumSqr 
    where f ((s0, s1), s2) = (s1/s0, (sqrt $ (s0*s2-s1*s1)/(s0*(s0-1)))/sqrt s0)

muLogNormalF :: Floating a => Fold a a
muLogNormalF = pure (/) <*> before sumF log <*> realLengthF

varLogNormalF ::Floating a => Fold a a
varLogNormalF = pure (-) <*> before meanF (square . log) <*> after muLogNormalF square
   where square x = x*x

sdLogNormalF ::Floating a => Fold a a
sdLogNormalF = fmap sqrt varLogNormalF


logNormalF :: Floating a => Fold a (a,a)
logNormalF = both muLogNormalF sdLogNormalF

--mean :: Fractional a => [a] -> a
--mean = cfoldl' meanF

{-regress :: (Tagged t) => [t (Double,Double)] -> (Double,Double) --tag of type num,num
regress vls = let xs = map (fst . getTag) vls
                  ys = map (snd . getTag) vls
                  xys = zip xs ys
                  mx = mean xs
                  my = mean ys
                  nume = sum $ map (\(x,y)->(x-mx)*(y-my)) xys
                  denom = sum $ map (square . (`sub` mx)) xs
                  slope = nume/denom
              in (slope,my-slope*mx)
-}

regressF :: Fold (Double, Double) (Double, Double)
regressF = post <$> (	 dotProdF `both` 
			(before sumF fst) `both` 
			(before sumF snd) `both` 
			(before sumF (square . fst)) `both` 
			realLengthF) 
	where post ((((dotp, sx), sy), sqrx), len) = let nume = dotp - sx*sy/len
							 denom = sqrx - square sx/len
							 slope = nume/denom
						     in (slope, sy/len - sx*slope/len)


nSumSumSqr :: Fractional a => Fold a ((a, a), a)
nSumSumSqr = (realLengthF `both` sumF `both` (before sumF square))

--regress = runStat regressF


--gamma from wikipedia "gamma distribution"

gammaF :: Fold Double (Double,Double)
gammaF = 
   let final mn lmn = 
           let s = log mn - lmn
               kapprox = (3-s+sqrt((s-3)**2+24*s))/(12*s)
               theta = recip kapprox * mn
           in (kapprox, theta)
   in pure final <*> meanF <*> before meanF log




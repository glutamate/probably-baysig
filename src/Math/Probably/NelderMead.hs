{-# LANGUAGE ScopedTypeVariables #-}

module Math.Probably.NelderMead where

import Numeric.LinearAlgebra

import Data.Ord
import Data.List
import Data.Maybe

import Debug.Trace

{-mean, m2 :: Vector Double
mean = fromList [45.1,10.3]

cov :: Matrix Double
cov = (2><2) $ [5.0, 1,
                1, 1.5]

invCov = inv cov

lndet = log $ det cov

pdf = multiNormalByInv lndet invCov mean

m2 = fromList [55.1,20.3]

main = runRIO $ do
  io $ print $ pdf mean
  ap <- nmAdaMet (defaultAM {nsam =1000000}) pdf (fromList [3.0,0.8])
  {-iniampar <- sample $ initialAdaMet 500 5e-3 (pdf) $ fromList [30.0,8.0]
  io $ print iniampar
  froampar <- runAndDiscard 5000 (show . ampPar) iniampar $ adaMet False (pdf)
  io $ print froampar
  io $ print $ realToFrac (count_accept froampar) / (realToFrac $ count froampar) -}
  let iniSim = genInitial (negate . pdf) 0.1 $ fromList [3.0,0.5]
  io $ mapM_ print iniSim
  let finalSim =  goNm (negate . pdf) 1 iniSim
  io $ print $ finalSim
  io $ print $ hessianFromSimplex (negate . pdf) finalSim -}

type Simplex = [(Vector Double, Double)]

centroid :: Simplex -> Vector Double
centroid points = scale (recip l) $ sum $ map fst points
    where l = fromIntegral $ length points

nmAlpha = 1
nmGamma = 2
nmRho = 0.5
nmSigma = 0.5

secondLast (x:y:[]) = x
secondLast (_:xs) = secondLast xs

replaceLast xs x = init xs ++ [x]

atLeastOne :: Double -> Double
atLeastOne x | isNaN x || isInfinite x = 1.0
             | x < -1.0 || x > 1.0 = realToFrac $ round x
             | x < 0 = -1.0
             | otherwise = 1.0

genInitial :: (Vector Double -> Double) -> [Int] -> (Int -> Double) -> Vector Double -> Simplex
genInitial f isInt h x0 = sim where
  n = length $ toList x0
  unit d = build n $ \jr -> let j = round jr in
                            if j /=d then 0.0 else if d `elem` isInt then atLeastOne $ h j*x0!d
                                                                     else h j*x0!d
  mkv d = with f $ x0 + unit d
  sim = (x0, f x0) : map mkv [0..n-1]

goNm :: (Vector Double -> Double) -> [Int] -> Double -> Int -> Int -> Simplex -> Simplex
goNm f' isInt tol nmin nmax sim' = go f' 0 $ sortBy (comparing snd) sim'  where
  go f i sim = let nsim = sortBy (comparing snd) $ (nmStep f isInt sim)
                   fdiff = abs $ snd (last nsim) - snd (head nsim)
               in case () of
                      _ |  (fdiff < tol && i>nmin) || i>nmax -> nsim
                        |  all (<0) (map snd sim) && any (>0) (map snd nsim) -> sim
                        |  any (isNaN) (map snd nsim) -> sim
                        |  otherwise   -> go f (i+1) nsim

goNmVerbose :: (Vector Double -> Double) -> [Int] -> Double -> Int -> Int -> Simplex -> Simplex
goNmVerbose f' isInt tol nmin nmax sim' = go f' 0 $ sortBy (comparing snd) sim' where
  go f i sim = let nsim = sortBy (comparing snd) $ (nmStep f isInt sim)
                   fdiff = trace ("1: "++ show (fst (head nsim)) ++ "\nlast: "++
                                show (fst (last nsim)) ++ "\n#"++show i++": "++
                                show (map snd nsim))
                            abs $ snd (last nsim) - snd (head nsim)
               in case () of
                    _ |  (fdiff < tol && i>nmin) || i>nmax -> nsim
                      |  all (<0) (map snd sim) && any (>0) (map snd nsim) -> sim
                      |  any (isNaN) (map snd nsim) -> sim
                      |  otherwise   -> go f (i+1) nsim


nmStep :: (Vector Double -> Double) -> [Int] -> Simplex -> Simplex
nmStep f isInt s0 = snext where
   x0 = centroid $ init s0
   xnp1 = fst (last s0)
   fxnp1 = snd (last s0)
   xr = x0 + nmAlpha * (x0 - xnp1)
   fxr = f xr
   fx1 = snd $ head s0
   snext = if fx1 <= fxr && fxr <= (snd $ secondLast s0)
              then replaceLast s0 (xr,fxr)
              else sexpand
   xe = x0 + nmGamma * (x0-xnp1)
   fxe = f xe
   sexpand = if fxr > fx1
                then scontract
                else if fxe < fxr
                        then replaceLast s0 (xe,fxe)
                        else replaceLast s0 (xr,fxr)
   xc = xnp1 + nmRho * (x0-xnp1)
   fxc = f xc
   scontract = if fxc < fxnp1
                  then replaceLast s0 (xc,fxc)
                  else sreduce
   sreduce = case s0 of
              p0@(x1,_):rest -> p0 : (flip map rest $ \(xi,_) -> with f $ x1+nmRho * (xi-x1))


with f x = (x, f x)

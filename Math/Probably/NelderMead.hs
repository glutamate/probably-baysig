{-# LANGUAGE ScopedTypeVariables #-}

module Math.Probably.NelderMead where

import Math.Probably.FoldingStats
import Numeric.LinearAlgebra

import Data.Ord
import Data.List
import Data.Maybe

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

hessianFromSimplex :: (Vector Double -> Double) -> [Int] -> [((Int, Int), Double)] -> Simplex -> (Vector Double, Matrix Double)
hessianFromSimplex f isInt fixed sim = 
  let mat :: [Vector Double]
      mat = toRows $ fromColumns $ map fst sim
      fsw ((y0, ymin),ymax) = (y0, max (ymax-y0) (y0-ymin))
      swings = flip map mat $ runStat (fmap fsw $ meanF `both` minFrom 1e80 `both` maxFrom (-1e80)) . toList 
      n = length swings
      xv = fromList $ map fst swings
      fxv = f  xv
      fixedpts = map fst fixed
      iswings i | i `elem` isInt = atLeastOne $snd $ swings!!i
                | otherwise = snd $ swings!!i
      funits d i | d/=i = 0
                 | i `elem` isInt = atLeastOne $ snd $ swings!!i
                 | otherwise = snd $ swings!!i 
      units = flip map [0..n-1] $ \d -> buildVector n $ funits d
      --http://www.caspur.it/risorse/softappl/doc/sas_docs/ormp/chap5/sect28.htm
      fhess ij@ (i,j) | ij `elem` fixedpts = fromJust $ lookup ij fixed 
                      | i>=j = 
                         ((f $ xv + units!!i + units!!j)
                          - (f $ xv + units!!i - units!!j)
                          - (f $ xv - units!!i + units!!j)
                          + (f $ xv - units!!i - units!!j) ) 
                          / (4*(iswings i) * (iswings j))
                      | otherwise = 0.0    
      hess1= buildMatrix n n fhess 
      hess2 = buildMatrix n n $ \(i,j) ->if i>=j then hess1@@>(i,j) 
                                                 else hess1@@>(j,i) 

  -- we probably  ought to make pos-definite
  -- http://www.mathworks.com/matlabcentral/newsreader/view_thread/103174
  -- posdefify in R etc  
  in (fromList (map (fst) swings), hess2)

atLeastOne :: Double -> Double
atLeastOne x | isNaN x || isInfinite x = 1.0
             | x < -1.0 || x > 1.0 = realToFrac $ round x
             | x < 0 = -1.0
             | otherwise = 1.0

genInitial :: (Vector Double -> Double) -> [Int] -> Double -> Vector Double -> Simplex
genInitial f isInt h x0 = sim where
  n = length $ toList x0
  unit d = buildVector n $ \j -> if j /=d then 0.0 else if d `elem` isInt then atLeastOne $ h*x0@>d 
                                                                          else h*x0@>d  
  mkv d = with f $ x0 + unit d
  sim = (x0, f x0) : map mkv [0..n-1] 

goNm :: (Vector Double -> Double) -> [Int] -> Double -> Simplex -> Simplex
goNm f' isInt tol sim' = go f' $ sortBy (comparing snd) sim' where
  go f sim = let nsim = sortBy (comparing snd) $ (nmStep f isInt sim)
             in if snd (last sim) - snd (head sim) < tol
                   then nsim
                   else go f nsim

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

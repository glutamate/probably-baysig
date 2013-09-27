{-# LANGUAGE TypeOperators, FlexibleContexts, DeriveDataTypeable, FlexibleInstances, GADTs, ScopedTypeVariables, UndecidableInstances, OverlappingInstances, MultiParamTypeClasses, NoMonomorphismRestriction, RankNTypes, BangPatterns #-}

module Target.Prelude where

{-
(S.Seed,  powInt, sample, primUnit, printC, solveODE,solveODEs, runTests, assertIt, expect, optimise, choose, listToVec, pack, Consolable(..), Askable(..), fillV, Always(..),Never(..),Mostly(..),SamplerDensity(..), runIO, observeSig, (!$!), packL, Plot(..),Radian(..),ObservedSignal(..), csvFile, withNats,  packV, vecToList, fillM, mdims)
-}

import qualified Math.Probably.Sampler as S
--import qualified Math.Probably.MALA as MALA
import qualified Math.Probably.NelderMead as NM
import qualified Math.Probably.PDF as PDF
--import Math.Probably.BFGS
import System.Random.Mersenne.Pure64
import Math.Probably.FoldingStats
--import Math.Probably.GlobalRandoms
import qualified Numeric.LinearAlgebra as L
import Data.List
import Data.Char (toLower)
import Data.Maybe (fromJust)
import Text.Printf
import Data.Record
import Data.Record.Combinators ((!!!))
import Data.Kind
import Data.TypeFun
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Generic as VG

import qualified Statistics.Math as SM
import Statistics.Test.KolmogorovSmirnov
import qualified Data.Vector.Unboxed as UV
import Control.Monad.Trans (lift)
import Data.Array

import Debug.Trace
import Control.Monad

import Data.List
import System.Exit
import System.IO.Unsafe
import Unsafe.Coerce
import Foreign.StablePtr
import Foreign.Storable
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.STRef
import Control.Monad.ST
import qualified Data.Vector.Storable.Mutable as VSM


runIO :: IO a -> IO a
runIO =   id -- silence

  --return $ S.Samples [k $ L.fromList init0]

choose_arr = array ((0,0), (500,500)) [((n,k), SM.choose n k) | n <- [0..500], k<- [0..500]]
choose n k | n< 0 || k < 0 = realToFrac $ SM.choose n k
           | n <501 && k<501 = realToFrac $ choose_arr!(n,k)
           | otherwise = realToFrac $ SM.choose n k

primUnit = \seed -> let (x, nseed) = randomDouble seed
                    in (realToFrac x, nseed)

--sample :: (Seed -> (a, Seed)) -> IO a
sample s = S.sampleIO s 

solveODE :: (Double -> Double -> Double) -> Double -> Double -> Double -> (Double -> Double)
solveODE f tmax dt y0 = y where
  ts = V.enumFromStepN 0 dt (round $ tmax/dt)
  ys = V.scanl intf y0 ts
  intf ylast t = ylast + dt * f ylast t
  y = pack dt 0 ys
--  y t = let ix = round $ t/dt
--        in  ys L.@> ix


nats :: [Int]
nats = [0..]

withNats :: [a] -> [(Int,a)] 
withNats = zip nats

solveODEs :: ([Double] -> Double -> [Double]) -> Double -> Double -> [Double] -> [Double -> Double]
solveODEs f tmax dt y0 = y where
  is = [0..(round $ tmax/dt)]
  yss = transpose $ scanl intf y0 is
--   yss = L.toRows $ L.trans $ L.fromRows $ map (L.fromList) $ scanl intf y0 is
  intf ylast t = zipWith (+) ylast $ map (dt *) $ f ylast $ (*dt) . realToFrac $ t
  y  = flip map yss $ \ys -> let yv = L.fromList ys in \t ->  let ix = round $ t/dt
                                                              in  yv L.@> ix

decide :: (Real b,Floating b, V.Storable b, RealFrac b) => (b -> (((V.Vector b) -> (((S.Prob a) -> (((((V.Vector b) -> ((a -> b)))) -> (V.Vector b))))))))
decide = \(tol) -> \((ini::V.Vector b)) -> \((prob::S.Prob a)) -> \((util::((V.Vector b) -> ((a -> b))))) -> (optimise tol ini) (\((vaction::V.Vector b)) -> expect (fmap (util vaction) prob))


traceIt s x = trace (s++": "++show x) x 

{-pmap :: (a -> b) -> ((c -> ((a,c))) -> (c -> ((b,c))))
pmap = \((f::a -> b)) -> \((sam::c -> ((a,c)))) -> bindP sam (\((x::a)) -> returnP (f x))

bindP :: (s -> ((a,s))) -> ((a -> (s -> ((b,s)))) -> (s -> ((b,s))))
bindP = \((f::s -> ((a,s)))) -> \((g::a -> (s -> ((b,s))))) -> \((s0::s)) -> let {((x::a),(s1::s)) = f s0} in g x s1

returnP :: a -> (b -> ((a,b)))
returnP = \((x::a)) -> \((s::b)) -> (x,s)
-}

{-for ::  Int -> (Int -> ((Int -> (s -> ((a,s)))) -> (s -> ((([a]),s)))))
for n m s | n < m = s n `bindP` \x -> 
                    for (n+1) m s `bindP` \xs -> 
                    returnP (x:xs) 
          | otherwise = ((s n) `bindP` (\v -> returnP (v:[]))) -}

--uniform a b = (\x->(realToFrac x)*(b-a)+a) `pmap` ran0

expect :: Fractional c => S.Prob c -> c
expect (S.Samples xs) = runStat meanF xs
expect s@(S.Sampler _) = unsafePerformIO $ do
  xs <- S.sampleNIO 100 s
  return $ expect $ S.Samples xs



optimise :: (Real a , Floating a, RealFrac a, V.Storable a) => a -> V.Vector a -> (V.Vector a -> a) -> V.Vector a
optimise nmtol ini fitfunPos = 

      let fitfun =  negate . realToFrac . fitfunPos . V.map realToFrac
          traceIt x = trace (show x) x
               --liftIO $ print (inivec, nmtol, fitfun inivec)
          inisimplex  = NM.genInitial fitfun [] (const 0.1) $ V.map realToFrac ini  
          --check here if differences is too small
          --liftIO $ print inisimplex
          nmRes = NM.goNm fitfun [] (realToFrac nmtol) 100 5000 $ inisimplex
                 --liftIO $ putStrLn $ "bye from optimse" ++ 
                  --                    show (NM.centroid nmRes)
      in V.map realToFrac $ NM.centroid nmRes

--decide :: Double -> ((Vector Double) -> ((S.Prob a) -> (((Vector Double) -> (a -> Double)) -> (Vector Double))))
--decide = \((tol::Double)) -> \((ini::Vector Double)) -> \((prob::S.Prob a)) -> \((util::(Vector Double) -> (a -> Double))) -> (optimise tol ini) (\((vaction::Vector Double)) -> expect (fmap (util vaction) prob))


xs !$! ix = if length xs > ix
               then xs !! ix
               else error $ "!$! out of boiunds: " ++show (ix, length xs)
               

powInt :: Double -> Int -> Double
powInt x p = x ^ p

{-binomial n p = 
   (for 1 n $ const $ pmap (<p) ran0) `bindP` \bools ->
   returnP $ length $ [t | t@True <- bools]

unormal =  ran0 `bindP` \u1 ->
           ran0 `bindP` \u2 -> 
           returnP $ sqrt(-2*log(u1))*cos(2*pi*u2) -}


csvFile :: FromCSV a => T.Text -> IO [a]
csvFile fnm = do
  lns <- fmap (tail . T.lines) $ TIO.readFile (T.unpack fnm)
  return $ map (fromCSV . T.splitOn (T.pack ",")) lns

class FromCSV a where
  fromCSV :: [T.Text] -> a

instance FromCSV (X (Id KindStar)) where
  fromCSV _ = X

instance (Name lab, FromCSVCell a, FromCSV (b (Id KindStar))) 
     => FromCSV ((b :& lab ::: a) (Id KindStar)) where
 fromCSV (t:ts) = (fromCSV ts) :& (name::lab) := (fromCSVcell t)


class FromCSVCell a where
  fromCSVcell :: T.Text -> a

instance FromCSVCell Double where
  fromCSVcell  = read . T.unpack

instance FromCSVCell T.Text where
  fromCSVcell  = id

printC :: Consolable a => T.Text -> a -> IO () 
printC s x = showC x >>= \s1-> putStrLn (T.unpack s++" => "++s1)

class Consolable a where
  showC :: a -> IO String
  showSamples :: [a] -> IO String

instance Consolable Double where
 showC x = return $ printf "%.3g" x
 showSamples xs =   let (mean,sd) = runStat meanSDF xs
                    in  return $ printf "%.3g +/- %.3g" mean sd

instance Consolable Bool where
 showC x = return $ show x
 showSamples bs = 
  let ntrue = length $ filter id bs
      ntotal = length bs
  in return $ show ntrue ++"/"++show ntotal

instance Consolable Int where
 showC x = return $ show x
 showSamples xs =   let (mean::Double,sd) = runStat meanSDF $ map realToFrac xs
                    in  return $ printf "%.3g +/- %.3g" mean sd

instance Consolable [Char] where
 showC x = return x
 showSamples (x:_) = return x

instance Consolable (Double -> Double) where
 showC _ = return "<signal>"
 showSamples _ = return "<signals>"


instance Consolable T.Text where
 showC x = return $ T.unpack x
 showSamples (x:_) = return $ T.unpack x

instance Consolable a => Consolable [a] where
 showC x = do ss <- mapM showC x 
              return $ "["++intercalate "," ss++"]"
 showSamples xs = do sxs <- mapM showC xs 
                     return $ "oneOf "++show sxs

instance (Consolable a, V.Storable a) => Consolable (L.Vector a) where
 showC x = do ss <- mapM showC $ L.toList x 
              return $ "<"++intercalate "," ss++">"
 showSamples xs = do sxs <- mapM showC xs 
                     return $ "oneOf "++show sxs


instance (Consolable a, Consolable b) => Consolable (a,b) where
 showC (x,y) = do sx <- showC x 
                  sy <- showC y 
                  return $ "("++sx++","++sy++")"


instance Consolable (X (Id KindStar)) where
 showC x = return ""
 showSamples _ = return ""

instance (Consolable a, Show lab, Consolable (b (Id KindStar))) 
     => Consolable ((b :& lab ::: a) (Id KindStar)) where
 showC (rest :& lab := val) = do
      vs <- showC val
      rest <- showC rest
      return $ if null rest
                  then uncap (show lab) ++ "=>"++vs
                  else uncap (show lab) ++ "=>"++vs ++ ";"++rest
 showSamples recs = do 
    let vs= map (\(rest :& lab := val) -> val) recs
    ss<- showSamples vs
    more <- showSamples (map getRest recs)
    return ("\n"++"   "++uncap (getLab (head recs)) ++ " => " ++ ss++more)
                 
uncap [] = []
uncap (c:cs) = toLower c : cs

getRest (rest :& lab := val) = rest
getLab (rest :& lab := val) = show lab

sampleFrom n sam@(S.Sampler _) =  do
  sequence $ replicate n $ sam
sampleFrom n sam@(S.Samples xs) =  return xs
  
instance Consolable a => Consolable (S.Prob a) where
 showC sam@(S.Sampler _) = do
  xs <- S.sampleIO $ sequence $ replicate 100 $ sam
  showSamples xs
 showC sam@(S.Samples xs) = do
  showSamples xs
---unormal = S.gaussD 0 1 
data Plot  = 
   Plot ([(T.Text,T.Text)]) (S.Prob ([Radian]))
   |PlotRow ([(T.Text,T.Text)]) ([Plot])
   |PlotColumn ([(T.Text,T.Text)]) ([Plot])
   |PlotGrid ([(T.Text,T.Text)]) Int Int ([(T.Text,Plot)])
   |PlotStack ([(T.Text,T.Text)]) ([(T.Text,Plot)])
data Radian  = 
   Lines ([(Double,Double)])
   |Points ([(Double,Double)])
   |Bars ([(Double,Double)])
   |Histogram ([Double])
   |Timeseries ((Double -> Double))
   |Options ([(T.Text,T.Text)]) ([Radian])
data ObservedSignal a = 
   ObservedSignal Double Double (V.Vector Double)
   |ObservedXYSignal (V.Vector ((Double,Double)))

observeSig :: (Double -> Double) -> ObservedSignal Double
observeSig  sig = unsafePerformIO $ do
    let ref = sig (0/0)
    deRefStablePtr $ unsafeCoerce ref 
      

pack :: Double -> Double -> L.Vector Double -> (Double -> Double)
pack dt t0 ys = unsafePerformIO $ do
     let obs = ObservedSignal dt t0 ys
     ptr <- newStablePtr obs
     return $ \t -> if isNaN t
                       then unsafeCoerce ptr
                       else let ix = round $ (t-t0)/dt
                            in  ys L.@> ix

packL :: Double -> Double -> [Double] -> (Double->Double)
packL dt t0 ys  = pack dt t0 (V.fromList ys)

--packV dt t0 ys t = let ix = round $ (t-t0)/dt
--                  in  ys V.! ix


data Always  = 
   Always (S.Prob Bool)
data Never  = 
   Never (S.Prob Bool)
data Mostly  = 
   Mostly (S.Prob Bool)
data SamplerDensity  = 
     SamplerDensity (S.Prob Double) (Double -> Double)
   | SamplerMass (S.Prob Int) (Int -> Double)
   | SamplesSame (S.Prob Double) (S.Prob Double)
class Assertable a where
  assertIt :: a -> S.Prob Bool

instance Assertable Bool where
  assertIt = return

instance Assertable Always where
  assertIt (Always s) = do
     bs <- sequence $ replicate 100 $ s
     return $ all id bs

instance Assertable Never where
  assertIt (Never s) = do
     bs <- sequence $ replicate 100 $ s
     return $ all (id . not) bs

instance Assertable Mostly where
  assertIt (Mostly s) = do
     bs <- sequence $ replicate 100 $ s
     let yeas = length $ filter id bs
     let nays = length $ filter not bs
     return $ yeas > nays 

ncdf = 2000

instance Assertable SamplerDensity where
  assertIt (SamplerDensity sam pdf) = do
      samples@(s0:_) <- sampleFrom 500 sam
      let (lo',hi') = runStat (both (minFrom s0) (maxFrom s0)) samples
          rng' = hi' - lo'
          lo = lo' - 3*rng'
          hi = hi' + 3*rng'
          rng = hi - lo
          dx = rng/ncdf
          xs = map ((+lo) . (*dx)) [0..(ncdf-1)]
          ys = map ((*dx) . exp . pdf) xs
          --prs = zip ys (tail ys)
          cumv =  listArray (0,(ncdf-1)) $ scanl (+) 0 ys
      let cdf x = cumv ! (round $ (x-lo)/dx)
      return $ kolmogorovSmirnovTestCdf cdf 0.05 (UV.fromList samples) == NotSignificant

  assertIt (SamplesSame sam1 sam2) = do
      samples1 <- sampleFrom 500 sam1
      samples2 <- sampleFrom 500 sam2
      return $ kolmogorovSmirnovTest2 0.05 (UV.fromList samples1) 
                                           (UV.fromList samples2)
                                    == NotSignificant


runTests :: [(String,S.Prob Bool)] -> IO ()
runTests tsts = do
  seed <- S.getSeedIO
  rt 0 0 seed tsts 
 where
  rt succ 0 _ [] = do
     putStrLn $ "All ("++show succ++") tests pass"
  rt succ fail _ [] = do
     putStrLn $ show fail++"/"++show (succ+fail)++" tests fail"
     exitWith $ ExitFailure 1
  rt s f seed ((nm, sam):rest) = do
     let (b,nseed) = sam1 seed sam
     if b
        then rt (s+1) f nseed rest
        else do putStrLn $ "FAIL: "++ nm
                rt s (f+1) nseed rest

sam1 seed (S.Samples xs) = (head xs, seed) 
sam1 seed (S.Sampler f) = f seed


--myRound :: Real a => a-> Int


listToVec xs = L.fromList xs

vecToList xs = L.toList xs

fillV = L.buildVector                        

fillM (n,m) f = L.buildMatrix n m f


svd m = let (m1, v, m2) = L.svd m
        in [m1, L.diag v, m2]

mdims m = (L.rows m, L.cols m)

class Askable a where
  ask :: Double -> Double -> [String] -> a -> IO [String]

instance Consolable a => Askable a where
   ask _ _ lns x = do
      ans  <- fmap lines $ showC x      
      return $ map (('>':) . (' ':)) $ init lns ++ 
                                       [last lns ++ " => " ++ head ans] ++ 
                                       tail' ans

tail' [] = []
tail' xs = tail xs

-- run-time reflection on records

{-class Has a where 
  has :: String -> a -> Bool

instance Has (X (Id KindStar)) where
  has _ _ = False

instance (Show lab, Name lab, Has (b (Id KindStar))) 
     => Has ((b :& lab ::: a) (Id KindStar)) where
  has nm (rest :& lab := val) = nm == show lab || has nm rest -}

instance L.Element Int 

class (Num a, Ord a, L.Element a) => BayNum a where
  unround :: a -> Double

instance BayNum Double where 
  unround = id

instance BayNum Int where 
  unround = realToFrac

bayError = error . T.unpack 

addSTRef ref x = modifySTRef ref (+x)

addSTRef ref !x = modifySTRef ref (+x)

consSTRef ref !x = modifySTRef ref (x:)

incrSTRef ref size = do
  x <- readSTRef ref
  let y = x+size
  y `seq` writeSTRef ref y
  return x

addMV vref ix x = do
  y <- VSM.read vref ix
  VSM.write vref ix (x+y)


showReal :: Double -> T.Text
showReal = T.pack . show

(*%) :: Double -> L.Matrix Double -> L.Matrix Double
(*%) = L.scale

transL :: [[a]] -> [[a]]
transL = transpose

unsafeFreeze = V.unsafeFreeze

vcons = V.cons 
slice = V.slice -- FIXME: are we sure this uses inicies correctly?
vmap = V.map
{-# LANGUAGE RankNTypes, BangPatterns, ScopedTypeVariables, OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad
import qualified Data.Text as T
import qualified Data.Vector.Storable as V
import Math.Probably.MCMC
import qualified Math.Probably.PDF as PDF
import Math.Probably.Sampler
import Strategy.RandomWalkMetropolis (rwm)
import Strategy.Hamiltonian (hmc)
import Strategy.NUTS (nuts)
import qualified Target.Regression
import qualified Target.Heston
import qualified Target.Logistic
import qualified Target.Prelude

test ::  IO (Prob Parameters)
test = do
  (posterior, postgrad,v2rec,inisam) <- Target.Heston.target
  iniv <- sampleIO inisam
  let rec = v2rec iniv
  sampleIO $ runChain inisam posterior postgrad 1 100 (hmc 100) 
  

altMain = do
  (posterior, postgrad,v2rec,inisam) <- Target.Heston.target
  iniv <- sampleIO inisam
  let rec = v2rec iniv
  chain <- sampleIO $ runChain inisam posterior postgrad 1 100 (hmc 100)
--  chain <- sampleIO $ runChain inisam posterior postgrad 1 100 rwm
  Target.Prelude.printC "initial" $ v2rec iniv
  Target.Prelude.printC "pars"    $ fmap v2rec chain
  return ()

-- | log-Rosenbrock function.
lRosenbrock :: RealFloat a => [a] -> a
lRosenbrock [x0, x1] = (-1) * (5 * (x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2)

lRosenbrockVec :: V.Vector Double -> Double
lRosenbrockVec xs = let [x0, x1] = V.toList xs
              in  lRosenbrock [x0, x1]

-- | Gradient of log-Rosenbrock.
glRosenbrock :: RealFloat a => [a] -> [a]
glRosenbrock [x, y] =
  let dx = 20 * x * (y - x ^ 2) + 0.1 * (1 - x)
      dy = -10 * (y - x ^ 2)
  in  [dx, dy]

glRosenbrockVec :: V.Vector Double -> V.Vector Double
glRosenbrockVec xs = let [x0, x1] = V.toList xs
               in  V.fromList . glRosenbrock $ [x0, x1]

-- | Return both the log-Rosenbrock function and its gradient at a point.
lRosenbrockWithGrad :: V.Vector Double -> (Double, V.Vector Double)
lRosenbrockWithGrad xs = (lRosenbrockVec xs, glRosenbrockVec xs)

-- | Sample the Rosenbrock density
rosenbrockWith :: Strategy a -> Prob (Prob Parameters)
rosenbrockWith s = runChain p0 lRosenbrockVec lRosenbrockWithGrad 10 100000 s
  where p0 = return $ V.fromList [1, 2]

-- | Return samples from the Rosenbrock density in IO.
runRosenbrock :: Strategy a -> IO [Parameters]
runRosenbrock s = do
  seed <- getSeedIO
  let peel   = runProb seed (rosenbrockWith s)
      peeled = head . map (runProb seed) $ peel
  return peeled

-- | Strip brackets and print something showable.
printWithoutBrackets :: Show a => a -> IO ()
printWithoutBrackets = putStrLn . filter (`notElem` "[]") . show

main :: IO ()
main = do
  zs <- runRosenbrock rwm
  mapM_ printWithoutBrackets (V.toList <$> zs)


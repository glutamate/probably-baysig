{-# LANGUAGE RankNTypes, ScopedTypeVariables, OverloadedStrings #-}

-- | Quick sanity checks for Markov chains.

module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector.Storable as V
import Math.Probably.MCMC
import qualified Math.Probably.PDF as PDF
import Math.Probably.Sampler
import Math.Probably.Types
import Strategy.MetropolisHastings

lRosenbrock :: V.Vector Double -> Double
lRosenbrock xs =
  let [x0, x1] = V.toList xs
  in  (-1) * (5 * (x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2)

glRosenbrock :: V.Vector Double -> V.Vector Double
glRosenbrock xs =
  let [x, y] = V.toList xs
      dx = 20 * x * (y - x ^ 2) + 0.1 * (1 - x)
      dy = -10 * (y - x ^ 2)
  in  V.fromList [dx, dy]

lHimmelblau :: V.Vector Double -> Double
lHimmelblau xs =
  let [x0, x1] = V.toList xs
  in  (-1) * ((x0 * x0 + x1 - 11) ^ 2 + (x0 + x1 * x1 - 7) ^ 2)

glHimmelblau :: V.Vector Double -> V.Vector Double
glHimmelblau xs =
  let [x, y] = V.toList xs
      quadFactor0 = x * x + y - 11
      quadFactor1 = x + y * y - 7
      dx = (-2) * (2 * quadFactor0 * x + quadFactor1)
      dy = (-2) * (quadFactor0 + 2 * quadFactor1 * y)
  in  V.fromList [dx, dy]

lBnn :: V.Vector Double -> Double
lBnn xs =
  let [x0, x1] = V.toList xs
  in  -0.5 * (x0 ^ 2 * x1 ^ 2 + x0 ^ 2 + x1 ^ 2 - 8 * x0 - 8 * x1)

glBnn :: V.Vector Double -> V.Vector Double
glBnn xs =
  let [x, y] = V.toList xs
      dx = -0.5 * (2 * x * y * y + 2 * x - 8)
      dy = -0.5 * (2 * x * x * y + 2 * y - 8)
  in  V.fromList [x, y]

lBeale :: V.Vector Double -> Double
lBeale xs
    | and [x0 >= -4.5, x0 <= 4.5, x1 >= -4.5, x1 <= 4.5]
        = negate $ (1.5   - x0 + x0 * x1) ^ 2
                 + (2.25  - x0 + x0 * x1 ^ 2) ^ 2
                 + (2.625 - x0 + x0 * x1 ^ 3) ^ 2
    | otherwise = - (1 / 0)
  where
    [x0, x1] = V.toList xs

glBeale :: V.Vector Double -> V.Vector Double
glBeale xs =
  let [x0, x1] = V.toList xs 
      dx = negate $ 2 * (1.5 - x0 + x0 * x1) * ((-1) + x1)
          + 2.25  * 2 * (2.25 - x0 + x0 * x1 ^ 2) * ((-1) + x1 ^ 2)
          + 2.625 * 2 * (2.2625 - x0 + x0 * x1 ^ 3) * ((-1) + x1 ^ 3)
      dy = negate $ 2 * (1.5 - x0 + x0 * x1) * x0
                  + 2 * (2.25 - x0 + x0 * x1 ^ 2) * 2 * x0 * x1
                  + 2 * (2.625 - x0 + x0 * x1 ^ 3) * 3 * x0 * x1 ^ 2
  in  V.fromList [dx, dy]

sanityCheck
  :: (V.Vector Double -> Double)
  -> (V.Vector Double -> V.Vector Double)
  -> V.Vector Double
  -> Transition Double
  -> IO ()
sanityCheck f g inisam s = do
  seed <- getSeedIO
  let target = createTargetWithGradient f g
      chain  = Chain inisam target (f inisam) (Map.empty)
      peel   = runProb seed $ trace 10000 s chain
      zs     = head . map (runProb seed) $ peel
      printWithoutBrackets = putStrLn . filter (`notElem` "[]") . show
  mapM_ (printWithoutBrackets . V.toList) zs

main :: IO ()
main =
  let p0 = V.fromList [0.0, 0.0]
  in  sanityCheck lRosenbrock glRosenbrock p0 (hamiltonian (Just 0.1) (Just 10))


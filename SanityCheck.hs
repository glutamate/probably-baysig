-- | Quick sanity checks for Markov chains.

module Main where

import qualified Data.Map as Map
import qualified Data.Vector.Storable as V
import Math.Probably.MCMC
import Math.Probably.Sampler
import Math.Probably.Types

lRosenbrock :: LogObjective
lRosenbrock (_, xs) =
  let [x0, x1] = V.toList xs
  in  (-1) * (5 * (x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2)

glRosenbrock :: Gradient
glRosenbrock xs =
  let [x, y] = V.toList xs
      dx = 20 * x * (y - x ^ 2) + 0.1 * (1 - x)
      dy = -10 * (y - x ^ 2)
  in  V.fromList [dx, dy]

lHimmelblau :: LogObjective
lHimmelblau (_, xs) =
  let [x0, x1] = V.toList xs
  in  (-1) * ((x0 * x0 + x1 - 11) ^ 2 + (x0 + x1 * x1 - 7) ^ 2)

glHimmelblau :: Gradient
glHimmelblau xs =
  let [x, y] = V.toList xs
      quadFactor0 = x * x + y - 11
      quadFactor1 = x + y * y - 7
      dx = (-2) * (2 * quadFactor0 * x + quadFactor1)
      dy = (-2) * (quadFactor0 + 2 * quadFactor1 * y)
  in  V.fromList [dx, dy]

lBnn :: LogObjective
lBnn (_, xs) =
  let [x0, x1] = V.toList xs
  in  -0.5 * (x0 ^ 2 * x1 ^ 2 + x0 ^ 2 + x1 ^ 2 - 8 * x0 - 8 * x1)

glBnn :: Gradient
glBnn xs =
  let [x, y] = V.toList xs
      dx = -0.5 * (2 * x * y * y + 2 * x - 8)
      dy = -0.5 * (2 * x * x * y + 2 * y - 8)
  in  V.fromList [dx, dy]

lBeale :: LogObjective
lBeale (_, xs)
    | and [x0 >= -4.5, x0 <= 4.5, x1 >= -4.5, x1 <= 4.5]
        = negate $ (1.5   - x0 + x0 * x1) ^ 2
                 + (2.25  - x0 + x0 * x1 ^ 2) ^ 2
                 + (2.625 - x0 + x0 * x1 ^ 3) ^ 2
    | otherwise = - (1 / 0)
  where
    [x0, x1] = V.toList xs

glBeale :: Gradient
glBeale xs =
  let [x0, x1] = V.toList xs 
      dx = negate $ 2 * (1.5 - x0 + x0 * x1) * ((-1) + x1)
          + 2.25  * 2 * (2.25 - x0 + x0 * x1 ^ 2) * ((-1) + x1 ^ 2)
          + 2.625 * 2 * (2.2625 - x0 + x0 * x1 ^ 3) * ((-1) + x1 ^ 3)
      dy = negate $ 2 * (1.5 - x0 + x0 * x1) * x0
                  + 2 * (2.25 - x0 + x0 * x1 ^ 2) * 2 * x0 * x1
                  + 2 * (2.625 - x0 + x0 * x1 ^ 3) * 3 * x0 * x1 ^ 2
  in  V.fromList [dx, dy]

sanityCheck f g inisam s = do
  seed <- getSeedIO
  let target = createTargetWithGradient f g
      chain  = Chain inisam target (f inisam) Nothing
      peel   = runProb seed $ trace 5000 s chain
      zs     = head . map (runProb seed) $ peel
      printWithoutBrackets = putStrLn . filter (`notElem` "fromList []()") . show
  mapM_ printWithoutBrackets zs

main :: IO ()
main =
  let p0 = (V.fromList [], V.fromList [0.0, 0.0])
  in  sanityCheck lRosenbrock glRosenbrock p0 nutsDualAveraging 


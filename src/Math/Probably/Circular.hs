module Math.Probably.Circular where

import Math.Probably.FoldingStats
import Numeric.LinearAlgebra
import Control.Applicative

--approximate method first mentioned http://en.wikipedia.org/wiki/Mean_of_circular_quantities
circularMean :: Fold Double Double
circularMean = before (after vmean getAngle) toCartesian where
  vmean = pure (scale) <*> after realLengthF recip <*> sumF
  getAngle v = atan2 (v@>1) (v@>0)
  toCartesian alpha = fromList [cos alpha, sin alpha]

--http://webspace.ship.edu/pgmarr/Geo441/Lectures/Lec%2016%20-%20Directional%20Statistics.pdf
circularDispersion :: Fold Double Double
circularDispersion = after (both xs ys) getR where
  xs = before sumF cos
  ys = before sumF sin
  getR (x,y) = sqrt (x*x + y*y)


--example for running statistic

runCircularMean :: [Double] -> Double
runCircularMean = runStat circularMean

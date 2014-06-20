module Math.Probably.Circular where

import Math.Probably.FoldingStats
import Numeric.LinearAlgebra
import Control.Applicative

--approximate method first mentioned http://en.wikipedia.org/wiki/Mean_of_circular_quantities
approxCircularMean :: [Double] -> Double
approxCircularMean = runStat (after vmean getAngle) . map toCartesian where
  vmean = pure (scale) <*> after realLengthF recip <*> sumF
  getAngle v = atan2 (v@>1) (v@>0)
  toCartesian alpha = fromList [cos alpha, sin alpha]


module Math.Probably.SDE where

import Math.Probably.Sampler

weiner :: Double -> Double -> Sampler [Double]
weiner dt tmax = do
   let npts = round $ tmax/dt
   etas <- fmap (map (*sqrt dt)) $ gaussManyUnitD npts 
   return $ flip scanl1 etas $ \last next -> next+last

eulerMaruyama :: Double -> Double 
                 -> (Double -> Double -> Double) 
                 -> (Double -> Double -> Double) 
                 -> [Double]
                 -> [Double]
eulerMaruyama dt x0 a b ws = xs where
  go x i (wthis:ws@(wnext:_)) = let t = realToFrac i*dt
                                    deltaw = wnext-wthis
                                    x1 = x + dt * a x t + b x t * deltaw
                                in x1 : go x1 (i+1) ws
  go _ _ _ = []
  xs = x0 : go x0 0 ws
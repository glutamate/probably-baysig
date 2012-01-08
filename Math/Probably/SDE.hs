module Math.Probably.SDE where

import Math.Probably.Sampler

unfoldM :: a -> (a -> Sampler a) -> Sampler [a]
unfoldM x0 f = do
   x1 <- f x0
   rest <- unfoldM x1 f
   return $ x1 : rest

weiner :: Double -> Double -> Sampler [Double]
weiner dt tmax = do
   let npts = round $ tmax/dt
   fmap (take npts) $ unfoldM 0 $ \wt -> gaussD wt $ sqrt dt


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
  xs = go x0 0 ws
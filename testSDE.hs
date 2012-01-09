module Main where 

import Math.Probably.Sampler
import Math.Probably.RandIO
import Math.Probably.FoldingStats

import Math.Probably.SDE

import Graphics.Gnewplot.Exec
import Graphics.Gnewplot.Types
import Graphics.Gnewplot.Style
import Graphics.Gnewplot.Panels
import Graphics.Gnewplot.Instances
import Graphics.Gnewplot.Histogram


const2 x y z = x

lastWeinerSampler dt = do
   ys <- weiner dt 1
   return $ last ys

lastSDESampler dt = do
   w <- weiner dt 1
   return $ last $ eulerMaruyama dt 0 a b w

a x t = 1
b x t = 1


main = runRIO $ do 
     let dt = 0.0001
     pts <- sample $ sequence $ replicate 1000 $ lastSDESampler dt
     --w <- sample $ fmap (eulerMaruyama dt 0 a b) $ weiner dt 1
     --io $ gnuplotOnScreen $ zip [(0::Double)..] w
     io $ print $ runStat meanSDF $ pts
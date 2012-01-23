module Main where 

import Math.Probably.Sampler
import Math.Probably.RandIO
import Math.Probably.FoldingStats

import Numeric.LinearAlgebra

import Math.Probably.Unscented

import Graphics.Gnewplot.Exec
import Graphics.Gnewplot.Types
import Graphics.Gnewplot.Style
import Graphics.Gnewplot.Panels
import Graphics.Gnewplot.Instances
import Graphics.Gnewplot.Histogram


dt = 0.1

f v =  fromList [t1, x0 + dt * sin (t1) + noisex]
  where t0 = v @> 0
        x0 = v @> 1
        noiset = v @> 2 
        noisex = v @> 3 
        t1 = t0+dt + noiset


f' v =  fromList [t1, x0 + dt * sin (t1) ]
  where t0 = v @> 0
        x0 = v @> 1
        t1 = t0+dt 



g v = fromList [t1+noiset, x1+noisex]
  where t1 = v @> 0
        x1 = v @> 1       
        noiset = v @> 2
        noisex = v @> 3

g' v = fromList [t1, x1]
  where t1 = v @> 0
        x1 = v @> 1       
  
procCov = (2><2) [0.001, 0, 
                  0,     0.01]

obsCov = (2><2) [0.01, 0, 
                  0,     0.01] 

--obsCov = (1><1)  [0.01]


main = runRIO $ do 
     xyvs <- sample $ simDynamical 200 f g procCov obsCov (fromList [0, 0.1])
--     pts <- sample $ sequence $ replicate 1000 $ lastSDESampler dt
     --w <- sample $ fmap (eulerMaruyama dt 0 a b) $ weiner dt 1
     let xhats = unscentedKalmanFilterAdditive f' g' procCov obsCov (fromList [0,0], ((2><2) [1,0,0,1])) $ map snd xyvs
     let get s f =  (s, Lines [] $ zip [(0::Double)..] $ map f xyvs)
     io $ gnuplotOnScreen $ get "y" ((@>0) . snd) :+: get "x" ((@>1) . fst) 
                            :+: ("xhat", Lines [] $ zip [(0::Double)..] $ map ((@>0) . fst)  xhats)
     io $ print $ take 10 $ map snd xyvs
--     io $ gnuplotOnScreen $ get "x" ((@>1) . fst)
--     io $ print $ runStat meanSDF $ pts
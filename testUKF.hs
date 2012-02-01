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


testUT = do
  let cov = (2><2) [0.1,0.08,0.08,0.1]
      mn = (2 |> [1,1])
      xform v = let [x,y] = toList v in fromList [x*x, y*y]
      mkpt = ((\[x,y] -> (x,y)) .toList)
  pts <- sample $ sequence $ replicate 1000 $ multiNormal mn cov
  let (mnU, covU, sigmapts) = unscentedTransform (mn,cov) xform
  let xformPts = map xform pts
  io $ gnuplotOnScreen $ map mkpt pts :+: map mkpt sigmapts --  :+: map mkpt xformPts
  io $ print mnU
  io $ print covU
  (mnE, covE) <- sample $ smellyTransform (multiNormal mn cov) 1000 xform
  io $ print mnE
  io $ print covE
  

main = runRIO $ do 
     xyvs <- sample $ simDynamical 2000 f g procCov obsCov (fromList [0, 0.1])
     --let xyvs = flip map xyvs' $ \(xv,yv) -> (xv,negate yv)
--     pts <- sample $ sequence $ replicate 1000 $ lastSDESampler dt
     --w <- sample $ fmap (eulerMaruyama dt 0 a b) $ weiner dt 1
     let xhats = unscentedKalmanFilterAdditive f' g' procCov obsCov (fromList [0,0], ((2><2) [1,0,0,1])) $ map snd xyvs
     let get s f =  (s, Lines [] $ zip [(0::Double)..] $ map f xyvs)
     io $ gnuplotOnScreen $ get "y" ((@>1) . snd) :+: get "x" ((@>1) . fst) 
                            :+: ("xhat", Lines [] $ zip [(0::Double)..] $ map ((@>1) . fst)  xhats)
     io $ print $ take 10 $ map snd xyvs 

     testUT
--     io $ gnuplotOnScreen $ get "x" ((@>1) . fst)
--     io $ print $ runStat meanSDF $ pts
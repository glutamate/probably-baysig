{-# LANGUAGE FlexibleInstances #-}

module Math.Probably.Visualise where

import Math.Probably.PlotR
import Math.Probably.Sampler
import qualified Math.Probably.PDF as PDF
import Math.Probably.StochFun
import Data.Unique
import TNUtils
import Data.List
import System.Directory

data WithPoints a = WithPoints Int a 
data WithRange a = WithRange Double Double a


instance Num a => PlotWithR (WithPoints (Sampler a)) where
    getRPlotCmd (WithPoints n sam) = do
      pts <- take n `fmap` runSamplerIO sam
      r <- (show . idInt . hashUnique) `fmap` newUnique
      let fnm = "/tmp/bugplot"++r
      writeFile fnm . unlines $ map (show) pts
      return $ RPlCmd { 
                   prePlot = [concat ["dat", r, " <- scan(\"", fnm, "\")"]], 
                   cleanUp = removeFile fnm,
                   plotArgs = [Histo $ "dat"++r]
                      }

instance PlotWithR (WithRange (PDF.PDF Double)) where
    getRPlotCmd (WithRange from to pdf) = do
      let dx = (to-from)/500
      let xs = [from,from+dx..to]
      let ys = map pdf xs
      r <- (show . idInt . hashUnique) `fmap` newUnique
      return $ RPlCmd { 
                   prePlot = [], 
                   cleanUp = return (),
                   plotArgs = [PLLines $ zip xs ys]
                      }


test :: IO ()
test = do --plot (WithPoints 10000 $ gauss 0 1)
          plot (WithRange (-5) 5 $ PDF.gauss (idDouble 0) 1)


idDouble :: Double -> Double
idDouble = id

--for PDF do special function not class ot get range
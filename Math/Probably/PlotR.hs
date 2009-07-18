module Math.Probably.PlotR where

import System.Cmd
import System.Directory
import Data.List
import System.Random

data RPlotCmd = RPlCmd {
--      plotData :: String,
      prePlot :: [String],
      plotArgs :: [String]
}

idInt :: Int -> Int
idInt = id

class PlotWithR a where
    getRPlotCmd :: a -> IO RPlotCmd
   
plotWithR :: PlotWithR a => a -> IO ()
plotWithR pl' = do
  pl <-  getRPlotCmd pl' 
  r <- (show . idInt) `fmap` randomRIO (0,10000)
  let rfile = "/tmp/bugplot"++r++".r"
  writeFile rfile $ unlines [
                 "x11(width=10,height=7)",
                 unlines $ prePlot pl,
                 "plot("++(head $ plotArgs pl)++")", 
                 "z<-locator(1)",
                 "q()"]
  system $ "R --vanilla --slave < "++rfile
  removeFile rfile
  return ()

newtype Points a = Points [a]

instance Num a => PlotWithR (Points a) where
    getRPlotCmd (Points xs) = 
        return $ RPlCmd {
                     prePlot = ["xs <- c("++(intercalate ", " $ map show xs)++")"],
                     plotArgs = ["xs"] }
instance (PlotWithR a, PlotWithR b) => PlotWithR (a,b) where
    getRPlotCmd (xs,ys) = do
      px <- getRPlotCmd xs
      py <- getRPlotCmd ys                          
      return $ RPlCmd {
                     prePlot = prePlot py++prePlot px,
                     plotArgs =  plotArgs px++ plotArgs py }
                               

--plotWithR :: V -> IO ()
--plotWithR (SigV t1 t2 dt sf) = do

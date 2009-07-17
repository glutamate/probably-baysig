module PlotR where

import System.Cmd
import System.Directory

data RPlotCmd = RPlCmd {
      plotData :: String,
      prePlot :: [String],
      plotArg :: String
}

class PlotWithR a where
    getRPlotCmd :: a -> IO RPlotCmd
   
plotWithR :: PlotWithR a => a -> IO ()

plotWithR :: V -> IO ()
plotWithR (SigV t1 t2 dt sf) = do
  (r::Int) <- randomRIO (0,10000)
  let datfile= "/tmp/bugplot"++show r
  let rfile = "/tmp/bugplot"++show r++".r"
  writeFile datfile $ unlines $ map (\t->show . unsafeVToDbl $ sf t) [0..round $ (t2-t1)/dt]
  writeFile rfile $ concat [
                 "x11(width=10,height=7)\n",
                 "dat <- ts(scan(\"", datfile, "\"), start=", show t1, ", frequency=", show (1/dt),")\n", 
                 "plot(dat)\n", 
                 "z<-locator(1)\n",
                 "q()"]
  system $ "R --vanilla --slave < "++rfile
  removeFile datfile
  removeFile rfile
  return ()
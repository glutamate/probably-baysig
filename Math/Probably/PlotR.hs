module Math.Probably.PlotR where

import System.Cmd
import System.Directory
import Data.List
import System.Random

data RPlotCmd = RPlCmd {
--      plotData :: String,
      prePlot :: [String],
      plotArgs :: [String]
} deriving Show

idInt :: Int -> Int
idInt = id

class PlotWithR a where
    getRPlotCmd :: a -> IO RPlotCmd
   
plotWithR :: PlotWithR a => a -> IO ()
plotWithR pl' = do
  pl <-  getRPlotCmd pl' 
  r <- (show . idInt) `fmap` randomRIO (0,10000)
  print pl
  let rfile = "/tmp/bugplot"++r++".r"
  let rlines = unlines [
                 "x11(width=10,height=7)",
                 unlines $ prePlot pl,
                 unlines $ map (\plArg-> "plot("++plArg++")") $ plotArgs pl,
                 "z<-locator(1)",
                 "q()"]
  writeFile rfile $ rlines
  putStrLn rlines
  system $ "R --vanilla --slave < "++rfile
  removeFile rfile
  return ()

newtype Points a = Points [a]

instance Num a => PlotWithR (Points a) where
    getRPlotCmd (Points xs) = do 
        r <- (show . idInt) `fmap` randomRIO (0,10000)
        return $ RPlCmd {
                     prePlot = ["xs"++r++" <- c("++(intercalate ", " $ map show xs)++")"],
                     plotArgs = ["xs"++r] }
instance (PlotWithR a, PlotWithR b) => PlotWithR (a,b) where
    getRPlotCmd (xs,ys) = do
      px <- getRPlotCmd xs
      py <- getRPlotCmd ys                          
      return $ RPlCmd {
                     prePlot = prePlot py++prePlot px,
                     plotArgs = plotArgs px++(map (++", add=TRUE") $ plotArgs py) }
                               
test = plotWithR (Points [1,2,3], Points [4,5,6])


--plotWithR :: V -> IO ()
--plotWithR (SigV t1 t2 dt sf) = do

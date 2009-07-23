{-# LANGUAGE TypeOperators #-}

module Math.Probably.PlotR where

import System.Cmd
import System.Directory
import Data.List
import Data.Unique
import TNUtils
import Control.Monad.Trans

data RPlotCmd = RPlCmd {
--      plotData :: String,
      prePlot :: [String],
      plotArgs :: [PlotLine],
      cleanUp :: IO ()
}

data PlotLine = TimeSeries String | Histo String | PLPoints [(Double,Double)] | PLLines [(Double,Double)]

plotLines :: [PlotLine] -> String
plotLines pls = let tss = [ts | TimeSeries ts <- pls ]
                    pts = [ts | PLPoints ts <- pls ]
                    lns = [ts | PLLines ts <- pls ]
                    hss = [ts | Histo ts <- pls ] in
                cond 
                [(nonempty tss, unlines ["ts.plot("++(intercalate "," tss)++")",
                                   unlines $ map lnsplot lns,
                                   unlines $ map ptplot pts]),
                 (nonempty hss, "hist("++head hss++")"),
                 (nonempty lns, unlines $ map (lnsOrPnts "plot" ", type=\"l\", xlab=\"xs\", ylab=\"ys\"") lns)]

    where lnsplot = lnsOrPnts "lines" ""
          ptplot = lnsOrPnts "points" ""
          lnsOrPnts cmd extra xsys  = let (xs,ys) = unzip xsys in 
                               cmd++"(c("++(intercalate "," $ map show xs)++"), c("++(intercalate "," $ map show ys)++")"++extra++")"

class PlotWithR a where
    getRPlotCmd :: a -> IO RPlotCmd
   
plotWithR :: PlotWithR a => a -> IO ()
plotWithR pl' = do
  pl <-  getRPlotCmd pl' 
  r <- (show. hashUnique) `fmap` newUnique
  --print pl
  let rfile = "/tmp/bugplot"++r++".r"
  let rlines = unlines [
                 "x11(width=10,height=7)",
                 unlines $ prePlot pl,
                 plotLines $ plotArgs pl,
                 "z<-locator(1)",
                 "q()"]
  writeFile rfile $ rlines
  --putStrLn rlines
  system $ "R --vanilla --slave < "++rfile
  removeFile rfile
  cleanUp pl
  return ()

plot :: (MonadIO m, PlotWithR p) => p -> m ()
plot = liftIO . plotWithR

newtype Points a = Points [a]

instance Real a => PlotWithR (Points a) where
    getRPlotCmd (Points xs) = do 
        r <- hashUnique `fmap` newUnique
        return $ RPlCmd {
                     prePlot = [],
                     plotArgs = [PLPoints $ zip [0..] $ map realToFrac xs], cleanUp = return () }

data a :+: b = a :+: b

instance (PlotWithR a, PlotWithR b) => PlotWithR (a :+: b) where
    getRPlotCmd (xs :+: ys) = do
      px <- getRPlotCmd xs
      py <- getRPlotCmd ys                          
      return $ RPlCmd {
                     prePlot = prePlot py++prePlot px,
                     plotArgs = plotArgs px++plotArgs py, 
                     cleanUp = cleanUp px >> cleanUp px }

instance (PlotWithR a) => PlotWithR [a] where
    getRPlotCmd xs = do
      pxs <- mapM getRPlotCmd xs
      return $ RPlCmd {
                     prePlot = concatMap prePlot pxs,
                     plotArgs = concatMap plotArgs pxs, 
                     cleanUp = mapM_ cleanUp pxs}

                               
--test = plotWithR (Points [1,2,3] :+: Points [4,5,6])


--plotWithR :: V -> IO ()
--plotWithR (SigV t1 t2 dt sf) = do

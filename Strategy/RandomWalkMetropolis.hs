module Strategy.RandomWalkMetropolis where

import Math.Probably.Sampler
import Math.Probably.MCMC
import Numeric.LinearAlgebra
import Control.Monad
import Data.Maybe


rwm = VStrategy rwmTrans rwmIni

rwmTrans posterior xi (sigma,(i, iaccept)) mpi = do
   let pi = fromMaybe (posterior xi) mpi
   xstar <- fmap fromList $ mapM (\x -> normal x sigma) $ toList xi
   let pstar =  posterior xstar
   let ratio = exp $ pstar - pi
   u <- unit
   let accept = u < ratio

   --diminishing adaptation:
   let sigmaNext = if accept then sigma*(min 1.4 $ 1+5.0/i)^3 else sigma*(max 0.7143 $ 1-5.0/i)

   return $ if accept 
               then ((xstar, (sigmaNext, (i+1, iaccept+1))), Just pstar)
               else ((xi, (sigmaNext, (i+1, iaccept))), Just pi) 
   

rwmIni _ = (0.1,(1.0, 0.0))


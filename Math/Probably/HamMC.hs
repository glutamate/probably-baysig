{-# LANGUAGE RankNTypes, BangPatterns, ScopedTypeVariables, FlexibleInstances #-}

module Math.Probably.HamMC where

import qualified Math.Probably.PDF as PDF
import Math.Probably.Sampler
import Control.Applicative

import Numeric.LinearAlgebra
import Text.Printf
import System.IO
import Data.Maybe

import Statistics.Test.KolmogorovSmirnov
import Statistics.Test.MannWhitneyU
import qualified Data.Vector.Unboxed as U
import qualified Control.Monad.State.Strict as S

import Debug.Trace
import Data.IORef
import Control.Spoon


data HMCPar = HMCPar { hpXi :: !(Vector Double),
                       hpL :: !Int,
                       hpPi :: !Double,
                       hpEpsilon :: !Double,
                       hpCount :: !Int,
                       hpAccept :: !Int,
                       hpFreezeEps :: Bool }
    deriving Show

hmc1 :: (Vector Double -> (Double,Vector Double)) 
         -> HMCPar
         -> Sampler HMCPar
hmc1 postGrad (HMCPar current_q l _ epsMean count accept freeze) = do
   let dims = dim current_q
       grad_u = negate . snd . postGrad
       u = negate . fst . postGrad
   current_p <- fmap fromList $ gaussManyUnitD dims
   eps <- uniform (epsMean*0.8) (epsMean*1.2)
   --the initial half momentum step 

   let p_half_step = current_p - scale (eps/2) (grad_u current_q)

   let step :: Int -> Vector Double -> Vector Double -> (Vector Double, Vector Double)
       step n p q  -- note that my l is Radford Neal's L+1
        | n == 0 = let qfinal = q + scale eps p -- only q not p update in last loop
                       pfinal = p - scale (eps/2) (grad_u qfinal) -- half momentum step at end
                   in (negate pfinal, -- negate momentum for symmetric proposal
                       qfinal)
        | otherwise =
            let q1 = q + scale eps p
                p1 = p - scale eps (grad_u q1)
            in step (n-1) p1 q1

   let (propose_p, propose_q) = step l p_half_step current_q
       current_U =  u current_q
       current_K =  (current_p `dot` current_p) / 2
       propose_U = u propose_q
       propose_K =  (propose_p `dot` propose_p) / 2
       ratio = exp $ current_U - propose_U + current_K - propose_K
       tr = max 1.0 $ realToFrac count

   u <- unitSample -- 0 to 1
   return $ if  u < ratio
               then HMCPar propose_q l propose_U (if freeze then epsMean else (min 2 $ 1+k_hmc/tr)*epsMean) (count+1) (accept+1) freeze
               else HMCPar current_q l current_U (if freeze then epsMean else (max 0.5 $ 1-k_hmc/tr)**1.8*epsMean) (count+1) (accept) freeze


traceAs s x = trace (s++" = "++show x) x


runHMC postgrad nsam hmc0 = go nsam hmc0 [] where
  go 0 hmcp xs = do io $ putStrLn $ "done with "++show hmcp
                    return (hmcp, xs)
  go n hmcp xs = do hmcnext <- sample $ hmc1 postgrad hmcp
                    if isNaN $ hpPi hmcnext
                       then return (hmcnext, [])
                       else do io $ do putStrLn $ show $ (hpCount hmcnext, 
                                                          hpPi hmcnext, 
                                                          hpEpsilon hmcnext)
                                       hFlush stdout
                               go (n-1) hmcnext $ hpXi hmcnext : xs


k_hmc = 2
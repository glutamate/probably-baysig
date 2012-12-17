{-# LANGUAGE RankNTypes, BangPatterns, ScopedTypeVariables, FlexibleInstances #-}

module Math.Probably.HamMC where

import Math.Probably.MCMC
import qualified Math.Probably.PDF as PDF
import Math.Probably.RandIO
import Math.Probably.FoldingStats
import Math.Probably.Sampler
import Control.Applicative

import Numeric.LinearAlgebra
import Numeric.AD
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
                       hpPi :: !Double,
                       hpL :: !Int,
                       hpEpsilon :: !Double,
                       hpCount :: !Int,
                       hpAccept :: !Int }

hmc1 :: (Vector Double -> (Double,Vector Double)) 
         -> HMCPar
         -> Sampler HMCPar
hmc1 postGrad (HMCPar current_q current_U l eps count accept) = do
   let dims = dim current_q
       grad_u = snd . postGrad
       u = fst . postGrad
   current_p <- fmap fromList $ gaussManyUnitD dims
   
   let step :: Int -> Vector Double -> Vector Double -> (Vector Double, Vector Double)
       step n p q  -- note that my l is Radford Neal's L+1
        | n == 0 = (negate $ p - scale (eps/2) (grad_u q),
                    q + scale eps p)
        | otherwise =
            let q1 = q + scale eps p
                p1 = p - scale eps (grad_u q)
            in step (n-1) p1 q1

   let (propose_p, propose_q) = step l (current_p - scale (eps/2) (grad_u current_q)) current_q
       current_K =  (current_p `dot` current_p) / 2
       propose_U = u propose_q
       propose_K =  (propose_p `dot` propose_p) / 2
       ratio = exp $ current_U - propose_U + current_K - propose_K
   u <- unitSample
   return $ if u < ratio
               then HMCPar propose_q propose_U l eps (count+1) (accept+1)
               else HMCPar current_q current_U l eps (count+1) (accept)

sumVec :: Vector Double -> Double
sumVec = foldVector (+) 0
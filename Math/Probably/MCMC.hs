{-# LANGUAGE ScopedTypeVariables #-}

module Math.Probably.MCMC where

import Control.Monad
import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Math.Probably.Sampler
import Math.Probably.Types
import Numeric.LinearAlgebra

interleave :: [Transition a] -> Transition a
interleave = foldl1 (>>) 

oneOfRandomly :: [Transition a] -> Transition a
oneOfRandomly ts = do
  j <- lift $ oneOf [0..(length ts - 1)]
  ts !! j

frequency :: [(Int, Transition a)] -> Transition a
frequency xs = lift (oneOf [1..tot]) >>= (`pick` xs) where
  tot = sum . map fst $ xs
  pick n ((k, v):vs)
    | n <= k    = v
    | otherwise = pick (n - k) vs

firstWithProb
  :: Double
  -> Transition a
  -> Transition a
  -> Transition a
firstWithProb p t0 t1 = do
  s <- lift $ bernoulli p
  if s then t0 else t1

trace
  :: Int
  -> Transition a
  -> Chain a
  -> Prob (Prob (Vector a))
trace n t o = do
  zs <- replicateM n t `evalStateT` o
  return $ Samples zs




-- type Gradient = Vector Double
-- type Parameters = Vector Double
-- type PosteriorDensity = Double
-- 
-- --type of function to calculate posterior and gradient 
-- type PostGradF =  Parameters -> (PosteriorDensity, Gradient)
-- 
-- --type of function to calculate posterior  
-- type PosteriorF =  Parameters -> PosteriorDensity
-- 
-- -- Strategies: a "strategy" is an algorithm for MCMC. example: random
-- -- walk metropolis, adaptive metropolis, mala, hamiltonian etc etc. 
-- 
-- -- type parameter `extra` is the extra information an algorithm gets to lug around.
-- --   use for e.g. adaptation parameters.
-- 
-- data Strategy extra
--       -- GStrategy: based on gradients. the first 
--     = GStrategy (GStrategyKernel extra) -- return postgrad for caching
--                 (Parameters -> extra) --initialise extra from initial val
--       --non-gradient based
--     | VStrategy (VStrategyKernel extra)
--                 (Parameters -> extra) --"extra" initialiser
-- 
-- --transition kernel for gradient-based strategies
-- type GStrategyKernel extra = 
--       PostGradF
--         -> Parameters --current point in parameter space
--         -> extra -- current "stuff"
--         -> Maybe (PosteriorDensity, Gradient) --previous gradient and posterior, if available
--         -> Prob ((Parameters, extra), --next point in param space
--                  Maybe (PosteriorDensity, Gradient))
-- 
-- --transition kernel for non-gradient-based strategies
-- type VStrategyKernel extra = 
--       PosteriorF
--         -> Parameters --current point
--         -> extra 
--         -> Maybe PosteriorDensity --prev posterior val for caching, if available
--         -> Prob ((Parameters, extra), Maybe PosteriorDensity) 
-- 
-- runChain :: Prob Parameters -- initial values 
--          -> PosteriorF      -- posterior function
--          -> PostGradF       -- posterior + gradient function
--          -> Int             -- how much should we thin the chain?
--          -> Int             -- how many samples do we need
--          -> Strategy a      -- the strategy
--          -> Prob (Prob Parameters) -- output. 
-- runChain inisam posterior postgrad thinn nsamples strat = do
--    ini <- inisam
--    let stuff0 = case strat of
--                   GStrategy _ inistuff -> inistuff ini
--                   VStrategy _ inistuff -> inistuff ini
--    chain <- case strat of
--       GStrategy kernel _ 
--          -> runChainG ini postgrad thinn nsamples stuff0 Nothing kernel
--       VStrategy kernel _ 
--          -> runChainV ini posterior thinn nsamples stuff0 Nothing kernel
--    return $ Samples chain
-- 
-- runChainG :: Parameters -> PostGradF -> Int -> Int -> a -> Maybe (PosteriorDensity, Gradient) -> GStrategyKernel a -> Prob [Parameters]
-- runChainG x0 postgrad _    0     _      _    _ = return [] -- done
-- runChainG x0 postgrad thin iters stuff0 mpg  gstrat  = do
--       ((x1::Parameters,stuff1), mpg1) <- gstrat postgrad x0 stuff0 mpg 
--       if (iters `rem` thin == 0)
--          then do rest <- runChainG x1 postgrad thin (iters-1) stuff1 mpg1 gstrat 
--                  return $ x1:rest
--          else runChainG x1 postgrad thin (iters-1) stuff1 mpg1 gstrat 
-- 
-- runChainV :: Parameters -> PosteriorF -> Int -> Int -> a -> Maybe PosteriorDensity -> VStrategyKernel a -> Prob [Parameters]
-- runChainV x0 posterior _    0     _      _    _ = return [] -- done
-- runChainV x0 posterior thin iters stuff0 mp  vstrat  = do
--       ((x1::Parameters,stuff1), mp1) <- vstrat posterior x0 stuff0 mp 
--       if (iters `rem` thin == 0)
--          then do rest <- runChainV x1 posterior thin (iters-1) stuff1 mp1 vstrat 
--                  return $ x1:rest
--          else runChainV x1 posterior thin (iters-1) stuff1 mp1 vstrat 
-- 
-- 

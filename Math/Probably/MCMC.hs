module Math.Probably.MCMC where

import Math.Probably.Sampler
import Numeric.LinearAlgebra

type Gradient = Vector Double
type Parameters = Vector Double

type PosteriorDensity = Double

--function to calculate posterior and gradient 
type PostGradF =  Parameters -> (PosteriorDensity, Gradient)


-- type parameter `extra` is the extra information an algorithm gets to lug around.
--   use for e.g. adaptation parameters.

data Strategy extra
      -- GStrategy: based on gradients. the first 
    = GStrategy ( PostGradF
                 -> Parameters --current point in parameter space
                 -> extra -- current "stuff"
                 -> (PosteriorDensity, Gradient) --previous gradient and posterior 
                 -> Prob ((Parameters, extra), --next point in param space
                          (Double, Vector Double))) -- return postgrad for caching
                (Vector Double -> extra) --initialise extra from initial val
      --non-gradient based
    | VStrategy ((Parameters -> PosteriorDensity) 
                 -> Parameters --current point
                 -> extra 
                 -> Double --prev posterior val for caching
                 -> Prob ((Vector Double, extra), Double)) 
                (Vector Double -> extra) --"extra" initialiser


rwm = VStrategy rwmTrans rwmIni

rwmTrans posterior xi (sigma,(i, iaccept)) pi = do
   xstar <- fmap fromList $ mapM (\x -> normal x sigma) $ toList xi
   let pstar =  posterior xstar
   let ratio = exp $ pstar - pi
   u <- unit
   let accept = u < ratio

   --diminishing adaptation:
   let sigmaNext = if accept then sigma*(min 1.4 $ 1+5.0/i)^3 else sigma*(max 0.7143 $ 1-5.0/i)

   return $ if accept 
               then ((xstar, (sigmaNext, (i+1, iaccept+1))), pstar)
               else ((xi, (sigmaNext, (i+1, iaccept))), pi) 
   

rwmIni _ = (0.1,(1.0, 0.0))

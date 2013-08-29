module Strategy.Hamiltonian where

import Math.Probably.Sampler
import Math.Probably.MCMC
import Numeric.LinearAlgebra
import Control.Monad
import Data.Maybe


hmc l = GStrategy (hmcTrans l) hmcIni

hmcTrans l postGrad current_q (epsMean,(i, iaccept)) mpigi = do
   let dims = dim current_q
       grad_u = negate . snd . postGrad
       u = negate . fst . postGrad
   current_p <- fmap fromList $ normalManyUnit dims
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
       tr = max 1.0 $ i

   u <- unit -- 0 to 1
   let accept = u < ratio

   let epsMeanNext 
         = if accept then (min 2 $ 1+k_hmc/tr)*epsMean
                     else (max 0.5 $ 1-k_hmc/tr)**1.8*epsMean
   return $ if accept
               then ((propose_q, (epsMeanNext, (i+1, iaccept+1))), 
                     Nothing) -- we can't be bothered to cache
               else ((current_q, (epsMeanNext, (i+1, iaccept))), 
                     Nothing) -- we can't be bothered to cache



k_hmc = 2

hmcIni _ = (0.1,(1.0, 0.0))


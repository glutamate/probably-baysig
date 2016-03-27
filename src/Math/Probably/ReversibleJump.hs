module Math.Probably.ReversibleJump where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Vector.Storable as V
import Math.Probably.Sampler
import Math.Probably.Types

-- NOTE This implementation is woefully incomplete.  Below are some notes I
--      had written up before I had to abandon it.

-- NB. On this reversible jump implementation
--
--     This is a *basic* reversible jump implementation.  It's difficult to
--     come up with a generic RJ moveset that works for every kind of model one
--     might be interested in.  Moves tend to be very model-dependent.
--
--     At present I have to make some assumptions about the structure of the
--     parameters passed in.  In particular I'm thinking about mixture models
--     in which the parameters are organized like:
--
--     ([j ..], [w_1, mu_11 .. mu_1d, ... , w_j, mu_j1.. mu_jd])
--
--     'j' is the number of mixture components, which is unknown.  'd' is the
--     dimension of the space that the mixtures live in.  So d = 1 for a
--     mixture of Gaussians on a line, 2 for a mixture of Gaussians on a plane,
--     and so on.  There are j(d + 1) parameters in total.
--
--     I'm also assuming that the covariances for the components are equal and
--     fixed.  This isn't a necessity, but it simplifies the structure that I
--     have to assume for the parameters.
--
--     We also need to assume that the posterior being targeted can support
--     arbitrarily adding components to the parameters.
--
--     Making this production-ready would probably require us to add further
--     structure to the parameters.  The next step might be a structure in
--     which mixture components are grouped appropriately:
--
--     ([j .. ], [[w_1, mu_11, .. , mu_1d], .. , [w_j, mu_j1, .. , mu_jd]])
--
--     A map structure might also be appropriate, and in the most general case
--     we might want something that can handle heterogeneous types.  But a
--     general implementation for reversible jump is a hard problem.
--
--     A different moveset is required for using reversible jump in jump
--     diffusion models; we may want to somehow incorporate a framework with
--     that here, or instead possibly break the RJ functionality into separate
--     modules for each kind of model.
--
--     An excellent reference re: jumping is here:
--
--     https://www.ma.utexas.edu/users/pmueller/class/422/handout1.pdf

-- | Legal jumps.
data Jump =
    Local
  | Split
  | Merge
  | Birth
  | Death
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Defined jumps.
jumps :: [Jump]
jumps = [minBound..maxBound] :: [Jump]

-- | Number of defined jumps.
nJumps :: Int
nJumps = length jumps

-- | The jump proposal distribution.
--
--   NB. it might be desirable to weight these, for example by putting more
--       weight on local jumps.
jump :: Prob Jump
jump = oneOf jumps

-- | Augment a transition operator by adding a potential dimensional jump.
addJump :: Transition a -> Transition a
addJump transition = do
  transition
  j <- lift jump
  transition `withJump` j

-- | Perform the appropriate jump.
withJump :: Transition a -> Jump -> Transition a
withJump t Local = jumpLocal t
withJump _ Split = jumpSplit
withJump _ Merge = jumpMerge
withJump _ Birth = jumpBirth
withJump _ Death = jumpDeath

-- | Don't change dimension, and instead perform a local transition with the
--   provided operator.
jumpLocal :: Transition a -> Transition a
jumpLocal = id

-- | Jump 'up' a dimension by splitting a model component in two.
jumpSplit :: Transition a
jumpSplit = do
  Chain current@(ds, cs) target v e <- get

  let b = 1
  u <- lift unit

  let components = V.head ds
      componentDimension = pred $ V.length cs `quot` components

  component <- lift $ oneOf [0..pred components]

  let weight = cs V.! component
      m  = V.slice (succ component) componentDimension cs
      m0 = V.map (subtract (u * b)) m
      m1 = V.map (+ (u * b)) m

  let proposed = (ds, cs) -- FIXME should be cs w/old component changed and new component added
  let jac      = 2 * b

  let pAccept  = splitAcceptProb target current proposed jac

  zc <- lift unit
  when (pAccept > zc) $ put (Chain proposed target v e)
  gets parameterSpacePosition


-- | Jump 'down' a dimension by merging two model components into one.
jumpMerge :: Transition a
jumpMerge = undefined

-- | Jump 'up' a dimension by adding a new model component.
jumpBirth :: Transition a
jumpBirth = do
  Chain current@(ds, cs) target v e <- get

  let numComponents = V.head ds
      muDim = pred $ V.length cs `quot` numComponents

      components = go [] cs where
        go acc vs
          | V.null vs = acc
          | otherwise =
              let (v, next) = V.splitAt (succ muDim) vs
              in  go (v : acc) next

  u     <- V.singleton <$> lift unit -- fixme; unit won't do, depends on other weights
  noise <- V.replicateM muDim (lift unormal)
  component <- V.drop 1 <$> (lift $ oneOf components)

  let newComponent = u V.++ (V.zipWith (+) component noise)
      proposed     = (ds, cs V.++ newComponent)
      jac = undefined

  let pAccept = birthAcceptProb target current proposed jac

  zc <- lift unit
  when (pAccept > zc) $ put (Chain proposed target v e)
  gets parameterSpacePosition

birthAcceptProb target current proposed jac =
      logObjective target proposed - logObjective target current
    + log (1 - 1 / fromIntegral m)
    - log (1 / fromIntegral m) - log undefined - log undefined -- q_u (norm), q_v (dirichlet?)
    + log jac


-- | Jump 'down' a dimension by removing an existing model component.
jumpDeath :: Transition a
jumpDeath = undefined -- do
--   Chain (ds, cs) target v e <- get
--   zc <- lift $ unit
--
--   let (diminished, u) = undefined -- V.splitAt (V.length cs - 1) cs ?
--       jac             = undefined -- can i roll with cases with this is 1 for now?
--       pAccept = acceptProb target (ds, cs) (ds, diminished) jac
--
--   when (pAccept > zc) $
--     put $ Chain (ds, diminished) target v e
--
--   gets parameterSpacePosition

-- | Probability of accepting a proposed split.
splitAcceptProb :: Target -> Parameters -> Parameters -> Double -> Double
splitAcceptProb target c@(_, current) p@(_, proposed) jac = exp . min 0 $
    logObjective target c - logObjective target c
  + log undefined - log undefined
  + log jac


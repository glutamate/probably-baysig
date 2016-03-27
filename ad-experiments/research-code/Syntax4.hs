{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Syntax4 where

import Control.Applicative
import Data.Graph
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Reify as R
import Data.Reify.Graph.CSE
import Data.Traversable
import Numeric.AD

data Expr a =
    Lit Double
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Var String
  deriving (Eq, Ord, Show, Functor)

data ExprF e =
    LitF Double
  | AddF e e
  | SubF e e
  | MulF e e
  | VarF String
  deriving (Eq, Ord, Show, Functor)

instance MuRef (Expr a) where
  type DeRef (Expr a)    = ExprF
  mapDeRef f (Add e0 e1) = AddF <$> f e0 <*> f e1
  mapDeRef f (Sub e0 e1) = SubF <$> f e0 <*> f e1
  mapDeRef f (Mul e0 e1) = MulF <$> f e0 <*> f e1
  mapDeRef _ (Lit v)     = pure (LitF v)
  mapDeRef _ (Var s)     = pure (VarF s)

instance Num (Expr a) where
  fromInteger = Lit . fromInteger
  e0 + e1     = Add e0 e1
  e0 - e1     = Sub e0 e1
  e0 * e1     = Mul e0 e1
  signum      = undefined
  abs         = undefined

eval (Lit d)     = d
eval (Add e0 e1) = eval e0 + eval e1
eval (Sub e0 e1) = eval e0 - eval e1
eval (Mul e0 e1) = eval e0 * eval e1
eval (Var v)     = error "kaboom"

-- can't preserve sharing during evaluation; *can* reify a graph
-- corresponding to the input, but it's as big as the input itself.  not
-- practical
treeE 0 = 1
treeE n = let shared = treeE (n - 1)
          in  shared + shared

square :: Num a => a -> a
square n = n * n

sumSquares :: Num a => [a] -> a
sumSquares [y, z] = square y + square z
sumSquares _ = undefined

x :: Expr a
x = Var "x"

xs :: [Expr a]
xs = [Var "x", Var "y"]

gss :: Num a => [a] -> [a]
gss = grad sumSquares

gs :: Num a => a -> a
gs = diff square

gsGraph :: (MuRef a, Num a) => a -> IO (R.Graph (DeRef a))
gsGraph = reifyGraph . gs

csedGsGraph
  :: (MuRef a, Ord (DeRef a Unique), Num a, Functor (DeRef a))
  => a -> IO (R.Graph (DeRef a))
csedGsGraph ys = cse <$> gsGraph ys

gssGraph :: (MuRef a, Num a) => [a] -> IO [R.Graph (DeRef a)]
gssGraph = traverse reifyGraph . gss

csedGssGraph
  :: (MuRef a, Ord (DeRef a Unique), Num a, Functor (DeRef a))
  => [a] -> IO [R.Graph (DeRef a)]
csedGssGraph ys = map cse <$> gssGraph ys

ex :: IO (R.Graph ExprF)
ex = csedGsGraph x

toGraph  :: (MuRef b, Ord (DeRef b Unique), Functor (DeRef b)) =>
     (a -> b) -> a -> IO (R.Graph (DeRef b))
toGraph f y = cse <$> (reifyGraph . f) y

--gradToGraph f ys = cse <$> (traverse reifyGraph . grad f) ys

-- need to consume the graphs produced

-- convert (k, v) = \y -> case v of
--   AddF l r -> ((+), k, [l, r])
--   SubF l r -> ((-), k, [l, r])
--   MulF l r -> ((*), k, [l, r])
--   LitF d   -> (\_ _ -> d, k, [])
--   VarF _   -> (\_ _ -> y, k, [])
-- 
-- toIntMap :: R.Graph t -> (Unique, IntMap (t Unique))
-- toIntMap (R.Graph ys u) = (u, IntMap.fromList ys)
-- 
-- data GraphInternal = GraphInternal {
--     reversed     :: Double -> [Vertex]
--   , vertexToNode :: Double -> Vertex -> (Double -> Double -> Double, Int, [Int])
--   , keyToVertex  :: Double -> Int -> Maybe Vertex
--   }
-- 
-- toGraph (R.Graph ys _) = graph where
--   combinedGraph y = graphFromEdges (map (($ y) . convert) ys)
--   verts = first   . combinedGraph
--   vToN  = second  . combinedGraph
--   kToV  = third   . combinedGraph
--   revd  = topSort . transposeG . verts
--   graph = GraphInternal revd vToN kToV
-- 
--   go [] l r _     = l + r
--   go (v:vs) l r y = 
-- 
-- 
--   go [] acc _ _ _     = acc
--   go (v:vs) acc ml mr y = 
--     let node = acc + ((first $ (vToN y) v) ml mr) -- go ... 0 0    to start
--     in  go vs acc 

-- lookup the node in the graph




--  init = head . reversed
--  node = first . 
  
-- now i need to use the reversed/topologicalally sorted graph in order to
-- reduce it

-- pop the leftmost vertex off the top of the list.  use that vertex to
-- get the node..







first (a, _, _)  = a
second (_, b, _) = b
third (_, _, c)  = c

-- topSort $ transposeG g
--
--
--   return $ 
--     (\x -> (\(b, _, _) -> b) $ graphFromEdges (map ($ x) $ converted))





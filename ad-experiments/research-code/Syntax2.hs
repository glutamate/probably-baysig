-- Forward-mode AD in a very simple symbolic language.  Here using GADTs.
--
-- Here Expr is a phantom type.  This allows me to derive Functor instances
-- for Expr and ExprF.
--
-- That in turn permits CSE on the graphs returned from Data.Reify.
-- 

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Syntax2 where

import Control.Applicative
import Data.Graph
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Reify
import qualified Data.Reify.Graph as R
import Data.Reify.Graph.CSE
import Data.Traversable
import Numeric.AD

data Expr a where
  Lit :: Double -> Expr a
  Add :: Expr a -> Expr a -> Expr a
  Sub :: Expr a -> Expr a -> Expr a
  Mul :: Expr a -> Expr a -> Expr a
  Var :: String -> Expr a
  deriving (Eq, Ord, Show, Functor)

-- instance Functor Expr where
--   fmap _ (Lit l)     = Lit l
--   fmap f (Add e0 e1) = Add (f e0) (f e1)
--   fmap f (Sub e0 e1) = Sub (f e0) (f e1)
--   fmap f (Mul e0 e1) = Mul (f e0) (f e1)
--   fmap _ (Var v)     = Var v

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

eval :: Expr a -> Double
eval (Lit j)     = j
eval (Add e0 e1) = eval e0 + eval e1
eval (Sub e0 e1) = eval e0 - eval e1
eval (Mul e0 e1) = eval e0 * eval e1
eval (Var _)     = error "evaluated unbound Var"

text :: Expr a -> String
text (Lit j)     = show j
text (Add e0 e1) = "(" ++ text e0 ++ " + " ++ text e1 ++ ")"
text (Sub e0 e1) = "(" ++ text e0 ++ " - " ++ text e1 ++ ")"
text (Mul e0 e1) = "(" ++ text e0 ++ " * " ++ text e1 ++ ")"
text (Var x)     = x

sumSquares :: Num a => [a] -> a
sumSquares [x, y] = square x + square y

square :: Num a => a -> a
square x = x * x

tree :: (Num a, Eq a) => a -> a
tree 0 = 1
tree n =
  let shared = tree (n - 1)
  in  shared + shared




-- compiling expression graph back to function

toIntMap :: R.Graph t -> (Unique, IntMap (t Unique))
toIntMap (R.Graph xs u) = (u, IntMap.fromList xs)




-- compileGraph (j, g) = do
--   a <- IntMap.lookup j g
--   case a of
--     AddF l r -> Add (compileGraph (l, g)) (compileGraph (r, g))
--     SubF l r -> Sub (compileGraph (l, g)) (compileGraph (r, g))
--     MulF l r -> Mul (compileGraph (l, g)) (compileGraph (r, g))
--     LitF d   -> Lit d
--     Var v    -> 
--   return a

-- invert sort of; turn the graph back into an expression
-- not that useful, really; don't want the expression anymore
-- want to *evaluate* the graph

-- evalGraph (top, g) = go 0 g where
--   go acc _ IntMap.empty = return acc
--   go acc node feed = do
--     ((k, v), next) <- IntMap.maxViewWithKey feed
--     let soFar = case v of
--                   LitF d   -> d
--                   AddF l r -> 
  

-- let blah = bottom value 
-- in  

-- 
-- example = do
--   gs <- traverse reifyGraph $ grad sumSquares [Var "x", Var "y"] 
--   let rgs  = map cse gs
--       (_, test) = toIntMap . head $ rgs
--       converted = map convert $ IntMap.toList test
--   return $ 
--     (\x -> (\(b, _, _) -> b) $ graphFromEdges (map ($ x) $ converted))

--example = do
--  gs <- traverse reifyGraph $ grad sumSquares [Var "x", Var "y"] 
--  let rgs  = map cse gs
--      test = toIntMap . head $ rgs
--  return $ test
--
--convert (k, v) = \x -> case v of
--  AddF l r -> ((+), k, [l, r])
--  SubF l r -> ((-), k, [l, r])
--  MulF l r -> ((*), k, [l, r])
--  LitF d   -> (\_ _ -> d, k, [])
--  VarF _   -> (\_ _ -> x, k, [])
--
--evalGraphUp x (top, g) = go 0 g where
--  go acc _ IntMap.empty = return acc
--  go acc node feed = do
--    ((k, v), next) <- IntMap.maxViewWithKey feed
--    soFar <- case v of
--               LitF d   -> d
--               VarF _   -> x
--               AddF l r -> evalGraphDown x l next + evalGraphDown x r next
--               SubF l r -> evalGraphDown x l next - evalGraphDown x r next
--               MulF l r -> evalGraphDown x l next * evalGraphDown x r next
--    go soFar (node - 1)
--
--evalGraphDown x from g = go 0 where
--  go acc k = do
--    v <- IntMap.lookup k g
--    result <- case v of
--      AddF l r -> evalGraphDown x l g + evalGraphDown x r g
--      SubF l r -> evalGraphDown x l g - evalGraphDown x r g
--      MulF l r -> evalGraphDown x l g * evalGraphDown x r g
--      VarF _   -> x
--      LitF d   -> d
--    return result
      



-- evalGraph (top, g) = go 0 g where
--   go acc _ IntMap.empty = return acc
--   go acc node feed = do
--     ((k, v), next) <- IntMap.maxViewWithKey feed
--     let soFar = case v of
--                   LitF d   -> d
--                   AddF l r -> 
  

-- alternatively; let x = max in 

-- want to interpret the graph as an AST again
-- traverse graph, build expression
-- do this bottom up?  top down?  how to do it efficiently?
--   i think bottom up.  start from the bottom and let blah = blah in ...
--
-- possible ways to do this:
--   - use the graph library and reify graph as Graph
--   - topologically sort graph
--   - evaluate graph, or do a parallel graph algorithm (graph coloring)





main :: IO ()
main = do
  putStrLn "observe body of non-looping function"
  print $ square (Var "x")

  putStrLn "\nobserve body of AD'd version"
  print $ diff square (Var "x")

  putStrLn "\nsharing in diff square"
  g <- reifyGraph $ diff square (Var "x")
  print g

  putStrLn "\nsharing in grad sumSquares, by parameter"
  gs <- traverse reifyGraph $ grad sumSquares [Var "x", Var "y"]
  mapM_ print gs

  putStrLn "\ncommon subexpression elimination in the above"
  mapM_ print (map cse gs)



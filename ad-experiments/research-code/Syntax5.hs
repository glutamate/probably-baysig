{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Syntax5 where

import Control.Applicative
import Data.Graph
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Reify as R
import Data.Reify.Graph.CSE
import Data.Traversable
import Numeric.AD

-- PHOAS
data Expr a =
    Lit Double
  | Var a
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Let (Expr a) (a -> Expr a)

let_ :: Expr a -> (Expr a -> Expr a) -> Expr a
let_ e0 e1 = Let e0 (\x -> e1 (Var x))

instance Num (Expr a) where
  fromInteger = Lit . fromInteger
  e0 + e1     = Add e0 e1
  e0 - e1     = Sub e0 e1
  e0 * e1     = Mul e0 e1
  signum      = undefined
  abs         = undefined

type ClosedExpr = forall a. Expr a

eval :: Expr Double -> Double
eval (Lit d)     = d
eval (Add e0 e1) = eval e0 + eval e1
eval (Sub e0 e1) = eval e0 - eval e1
eval (Mul e0 e1) = eval e0 * eval e1
eval (Var v)     = v
eval (Let e0 e1) = eval (e1 (eval e0))

treeE :: (Num a, Eq a) => a -> Expr b
treeE 0 = 1
treeE n = let_ (treeE (n - 1)) (\shared -> shared + shared)

textE :: ClosedExpr -> String
textE e = go e 0 where
  go (Lit j)     _ = show j
  go (Add e0 e1) c = "(" ++ go e0 c ++ " + " ++ go e1 c ++ ")"
  go (Sub e0 e1) c = "(" ++ go e0 c ++ " - " ++ go e1 c ++ ")"
  go (Mul e0 e1) c = "(" ++ go e0 c ++ " * " ++ go e1 c ++ ")"
  go (Var x) _     = x
  go (Let e0 e1) c = "(let " ++ v ++ " = " ++ go e0 (c + 1) ++
                     " in " ++ go (e1 v) (c + 1) ++ ")"
    where v = "v" ++ show c

square x = x * x

sharedSquare x = let_ x (\shared -> shared * shared)

-- gs :: Num a => a -> a
-- gs = diff square


-- data ExprF e =
--     LitF Double
--   | AddF e e
--   | SubF e e
--   | MulF e e
--   | VarF String
--   deriving (Eq, Ord, Show, Functor)
-- 
-- instance MuRef (Expr a) where
--   type DeRef (Expr a)    = ExprF
--   mapDeRef f (Add e0 e1) = AddF <$> f e0 <*> f e1
--   mapDeRef f (Sub e0 e1) = SubF <$> f e0 <*> f e1
--   mapDeRef f (Mul e0 e1) = MulF <$> f e0 <*> f e1
--   mapDeRef _ (Lit v)     = pure (LitF v)
--   mapDeRef _ (Var s)     = pure (VarF s)
-- 
-- square :: Num a => a -> a
-- square n = n * n
-- 
-- sumSquares :: Num a => [a] -> a
-- sumSquares [y, z] = square y + square z
-- sumSquares _ = undefined
-- 
-- 

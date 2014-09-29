{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Syntax6 where

import Control.Applicative
import Data.Graph
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Reify as R
import Data.Reify.Graph.CSE
import Data.Traversable
import Numeric.AD

-- HOAS
data Expr =
    Lit Double
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Let Expr (Expr -> Expr)

instance Num Expr where
  fromInteger = Lit . fromInteger
  e0 + e1     = Add e0 e1
  e0 - e1     = Sub e0 e1
  e0 * e1     = Mul e0 e1
  signum      = undefined
  abs         = undefined

eval :: Expr -> Double
eval (Lit d)     = d
eval (Add e0 e1) = eval e0 + eval e1
eval (Sub e0 e1) = eval e0 - eval e1
eval (Mul e0 e1) = eval e0 * eval e1
eval (Var v)     = error "not allowed"
eval (Let e0 e1) = let shared = Lit (eval e0) -- can't analyzed shared expression
                   in  eval (e1 shared)

treeE :: Double -> Expr
treeE 0 = 1
treeE n = Let (treeE (n - 1)) (\shared -> shared + shared)

square x = x * x

-- ex: map eval $ grad sumSquares $ [1, 2]
sumSquares [x, y] = square x + square y

-- textE :: Expr -> String
-- textE e = go e 0 where
--   go (Lit j)     _ = show j
--   go (Add e0 e1) c = "(" ++ go e0 c ++ " + " ++ go e1 c ++ ")"
--   go (Sub e0 e1) c = "(" ++ go e0 c ++ " - " ++ go e1 c ++ ")"
--   go (Mul e0 e1) c = "(" ++ go e0 c ++ " * " ++ go e1 c ++ ")"
--   go (Var x) _     = x
--   go (Let e0 e1) c = "(let " ++ v ++ " = " ++ go e0 (c + 1) ++
--                      " in " ++ go (e1 v) (c + 1) ++ ")"
--     where v = "v" ++ show c


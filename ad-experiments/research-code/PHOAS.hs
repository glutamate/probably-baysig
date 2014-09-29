{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module PHOAS where

import Utils

data Expr a =
    Lit Int
  | Var a
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Let (Expr a) (a -> Expr a)

-- note: can still use haskell's let provided use of data-reify;
-- see andres's 'haskell for (e)dsls'
let_ :: Expr a -> (Expr a -> Expr a) -> Expr a
let_ e0 e1 = Let e0 (\x -> e1 (Var x))

instance Num (Expr a) where
  fromInteger = Lit . fromInteger
  e0 + e1     = Add e0 e1
  e0 - e1     = Sub e0 e1
  e0 * e1     = Mul e0 e1
  signum      = undefined
  abs         = undefined

instance Eq (Expr Int) where
  e0 == e1 = eval e0 == eval e1

type ClosedExpr = forall a. Expr a

-- note here that we can't case-analyze the shared expression
eval :: Expr Int -> Int
eval (Lit d)     = d
eval (Var v)     = v
eval (Add e0 e1) = eval e0 + eval e1
eval (Sub e0 e1) = eval e0 - eval e1
eval (Mul e0 e1) = eval e0 * eval e1
eval (Let e f)   = eval (f (eval e))

inline :: Expr (Expr a) -> Expr a
inline (Lit d)     = Lit d
inline (Var x)     = x
inline (Add e0 e1) = Add (inline e0) (inline e1)
inline (Sub e0 e1) = Sub (inline e0) (inline e1)
inline (Mul e0 e1) = Mul (inline e0) (inline e1)
inline (Let e f)   = inline (f (inline e))

tree :: (Num a, Eq a) => a -> Expr b
tree 0 = 1
tree n = let_ (tree (n - 1)) (\shared -> shared + shared)

autoTree :: (Num b, Eq b, Mode t, Scalar t ~ Expr a) => b -> t
autoTree 0 = auto 1
autoTree n = auto $ let_ (tree (n - 1)) (\shared -> shared + shared)

-- i think i need to do something like this to extract the derivative as a
-- symbolic expression; perhaps one using a simpler syntax.
--
-- this type of function allows one to observe sharing, so observing sharing
-- here is useful for i.e. assembling things like graphs.
--
-- in essence, compile this expression type to a graph, then evaluate the graph
text :: ClosedExpr -> String
text e = go e 0 where
  go (Lit j)     _ = show j
  go (Add e0 e1) c = "(" ++ go e0 c ++ " + " ++ go e1 c ++ ")"
  go (Sub e0 e1) c = "(" ++ go e0 c ++ " - " ++ go e1 c ++ ")"
  go (Mul e0 e1) c = "(" ++ go e0 c ++ " * " ++ go e1 c ++ ")"
  go (Var x) _     = x
  go (Let e0 e1) c = "(let " ++ v ++ " = " ++ go e0 (c + 1) ++
                     " in " ++ go (e1 v) (c + 1) ++ ")"
    where v = "v" ++ show c


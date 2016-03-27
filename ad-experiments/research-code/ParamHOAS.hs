{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module ParamHOAS where

import Data.Map (Map)
import qualified Data.Map as Map
import Utils

type Environment a = Map String a

data Expr a =
    Lit a
  | Var String
  | Add (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Let (Expr a) (Expr a -> Expr a)

instance Num a => Num (Expr a) where
  fromInteger = Lit . fromInteger
  (+)         = Add
  (*)         = Mul

eval :: Num a => Expr a -> Environment a -> a
eval (Lit d) _       = d
eval (Var v) env     = close v env
eval (Add e0 e1) env = eval e0 env + eval e1 env
eval (Mul e0 e1) env = eval e0 env * eval e1 env
eval (Let e0 e1) env =
  let shared = Lit (eval e0 env)
  in  eval (e1 shared) env

close :: String -> Environment a -> a
close v = fromJust . Map.lookup v

test :: Num a => Expr a
test = Let (Var "x") (\v -> Add (Mul v v) (Lit 1))

exEnv :: a -> Environment a
exEnv x = Map.fromList [("x", x)]



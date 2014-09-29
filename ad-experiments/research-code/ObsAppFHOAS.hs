{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module ObsAppFHOAS where

import Utils

-- BASIC HOAS

-- data Expr =
--     Lit Int
--   | Add Expr Expr
--   | Let Expr (Expr -> Expr)
  
instance Num Expr where
  fromInteger = Lit . fromInteger
  (+) = Add

-- eval :: Expr -> Int
-- eval (Lit d)         = d
-- eval (Add e0 e1)     = eval e0 + eval e1 
-- eval (Let e0 e1)     =
--   let shared = Lit (eval e0)
--   in  eval (e1 shared)
-- 
-- test :: Int -> Expr
-- test d = Let (Lit d) (\x -> Add x (Lit 1))

-- EXPANDED HOAS

data Expr =
    Lit Int
  | Var String
  | Add Expr Expr
  | Let Expr (Expr -> Expr)
  | Lam String Expr
  
eval :: Expr -> [(String, Int)] -> Int
eval (Lit d) _       = d
eval (Var v) env     = fromJust $ lookup v env
eval (Add e0 e1) env = eval e0 env + eval e1 env
eval (Let e0 e1) env =
  let shared = Lit (eval e0 env)
  in  eval (e1 shared) env


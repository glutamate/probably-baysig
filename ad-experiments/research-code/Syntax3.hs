-- Forward-mode AD in a very simple symbolic language.  Here using PHOAS.

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleInstances #-}

module Syntax3 where

data Expr a =
    Lit Double
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Var a
  | Let (Expr a) (a -> Expr a)
  | Lam (a -> Expr a)
  | App (Expr a) (Expr a)

instance Show a => Show (Expr a) where
  show (Lit j)     = "Lit " ++ show j
  show (Add e0 e1) = "Add (" ++ show e0 ++ ") (" ++ show e1 ++ ")"
  show (Sub e0 e1) = "Sub (" ++ show e0 ++ ") (" ++ show e1 ++ ")"
  show (Mul e0 e1) = "Mul (" ++ show e0 ++ ") (" ++ show e1 ++ ")"
  show (Var a)     = "Var " ++ show a
  show (Let e _)   = "let (" ++ show e ++ ") in <function>"
  show (Lam _)     = "<function>"
  show (App e0 e1) = "App (" ++ show e0 ++ ") ( " ++ show e1 ++ ")" 

instance Num (Expr a) where
  fromInteger = Lit . fromInteger
  e0 + e1     = Add e0 e1
  e0 - e1     = Sub e0 e1
  e0 * e1     = Mul e0 e1
  signum      = undefined
  abs         = undefined

data Value =
    N Double
  | F (Value -> Value)

instance Show Value where
  show (N j) = show j
  show (F _) = "<function>"

eval :: Expr Value -> Value
eval (Lit j)     = N j
eval (Add e0 e1) = add (eval e0) (eval e1)
eval (Sub e0 e1) = sub (eval e0) (eval e1)
eval (Mul e0 e1) = mul (eval e0) (eval e1)
eval (Var x)     = x
eval (Let e0 e1) = eval (e1 (eval e0))
eval (Lam e)     = F (\v -> eval (e v))
eval (App e0 e1) = app (eval e0) (eval e1)

add :: Value -> Value -> Value
add (N m) (N n) = N (m + n)

sub :: Value -> Value -> Value
sub (N m) (N n) = N (m - n)

mul :: Value -> Value -> Value
mul (N m) (N n) = N (m * n)

app :: Value -> Value -> Value
app (F f) v = f v

data Dual a = Dual {
    primal  :: Expr a
  , tangent :: Expr a
  } deriving Show

instance Num (Dual a) where
  fromInteger                 = constD . fromInteger
  (Dual e0 e1) + (Dual y0 y1) = Dual (e0 + y0) (e1 + y1)
  (Dual e0 e1) * (Dual y0 y1) = Dual (e0 * y0) (e0 * y1 + y0 * e1)
  negate (Dual e0 e1)         = Dual (negate e0) (negate e1)
  signum _                    = undefined
  abs _                       = undefined

constD :: Expr a -> Dual a
constD x = Dual x (Lit 0)

idD :: Expr a -> Dual a
idD x = Dual x (Lit 1.0)

evalDual :: Dual Value -> (Value, Value)
evalDual (Dual e0 e1) = (eval e0, eval e1)

embed :: (Dual a -> c) -> Expr a -> c
embed f = f . idD

simpleInc :: Num a => a -> a
simpleInc x = x + 1

example :: Num a => a -> a
example x = x ^ 2 + 2 * x + 1

square :: Num a => a -> a
square x = x * x

tree :: (Num a, Eq a) => a -> Expr b
tree 0 = Lit 1
tree n =
  let shared = tree (n - 1)
  in  shared + shared

-- main :: IO ()
-- main = 
--   print $ simpleInc (Var "x")         -- observes body of non-looping function
--   -- g <- reifyGraph (tree (Var "x")) -- can't observe loop structure
--   -- print g




{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Henrik where

import Utils
import System.IO.Unsafe

data Expr a =
    Lit a
  | Var String
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  deriving (Eq, Show)

instance Num a => Num (Expr a) where
  fromInteger = Lit . fromInteger
  e0 + e1     = Add e0 e1
  e0 - e1     = Sub e0 e1
  e0 * e1     = Mul e0 e1
  signum      = undefined
  abs         = undefined

close :: String -> Expr a -> Expr a -> Expr a
close x e0 = (`closeAux` e0) where
  closeAux _ e@(Lit _) = e
  closeAux v e@(Var y)
    | x == y    = v
    | otherwise = e 
  closeAux v (Add e1 e2) = Add (closeAux v e1) (closeAux v e2)
  closeAux v (Sub e1 e2) = Sub (closeAux v e1) (closeAux v e2)
  closeAux v (Mul e1 e2) = Mul (closeAux v e1) (closeAux v e2)

--autoClose :: String -> Expr a -> Expr a -> Expr a
autoClose x e0 = (`closeAux` e0) where
  closeAux _ e@(Lit _) = auto e
  closeAux v e@(Var y)
    | x == y    = v
    | otherwise = e 
  closeAux v (Add e1 e2) = Add (closeAux v e1) (closeAux v e2)
  closeAux v (Sub e1 e2) = Sub (closeAux v e1) (closeAux v e2)
  closeAux v (Mul e1 e2) = Mul (closeAux v e1) (closeAux v e2)

eval :: Num a => Expr a -> a
eval (Lit d)     = d
eval (Var _)     = error "stuck"
eval (Add e0 e1) = eval e0 + eval e1
eval (Sub e0 e1) = eval e0 - eval e1
eval (Mul e0 e1) = eval e0 * eval e1

-- fuse closing with evaluation
evalClose :: Mode t => String -> Expr (Scalar t) -> t -> t
evalClose x e0 = (`ec` e0) where
  ec _ (Lit d) = auto d
  ec v (Var y) 
    | x == y    = v
    | otherwise = error "kaboom"
  ec v (Add e1 e2) = ec v e1 + ec v e2
  ec v (Sub e1 e2) = ec v e1 - ec v e2
  ec v (Mul e1 e2) = ec v e1 * ec v e2

-- so, our expression:
test :: Num a => Expr a
test = 8 * Var "x" ^ 10

-- our reified function
f :: Mode t => t -> t
f = evalClose "x" test

fx e = diff $ evalClose "x" e

-- our reified derivative
g :: Num a => Expr a -> Expr a
g = diff f

-- our optimized, reified derivative
gOpt :: Num a => Expr a -> Graph (ExprF a)
gOpt = unsafePerformIO . reifyGraph . g

testo :: (MuRef s, Num s) => Expr s -> s -> Graph (DeRef s)
testo e v = unsafePerformIO . reifyGraph $ diff (evalClose "x" e) v

toGraphDerivative
  :: (MuRef s, Num s)
  => Expr s -> String -> s -> Graph (DeRef s)
toGraphDerivative expr x v =
  unsafePerformIO . reifyGraph $ diff (evalClose x expr) v














-- testo e v = unsafePerformIO . reifyGraph . diff $ evalClose "x" e v


-- so the process here has been to convert an unclosed expression into a
-- differentiable function, which i can then optimize via data-reify. 
-- evaluators can then take care of the resulting AST more efficiently than
-- could be done otherwise.


data ExprF a e =
    LitF a
  | AddF e e
  | SubF e e
  | MulF e e
  | VarF String
  deriving (Eq, Ord, Show, Functor)

instance MuRef (Expr a) where
  type DeRef (Expr a)    = ExprF a
  mapDeRef h (Add e0 e1) = AddF <$> h e0 <*> h e1
  mapDeRef h (Sub e0 e1) = SubF <$> h e0 <*> h e1
  mapDeRef h (Mul e0 e1) = MulF <$> h e0 <*> h e1
  mapDeRef _ (Lit v)     = pure (LitF v)
  mapDeRef _ (Var s)     = pure (VarF s)

{-
To test, e.g.:

Test> :t close "x" (Add (Lit 1) (Var "y"))
close "x" (Add (Lit 1) (Var "y")) :: Num a => Expr a -> Expr a

Test> close "x" (Add (Lit (1 :: Int)) (Var "y")) (Var "z")
Add (Lit 1) (Var "y")

Test> close "y" (Add (Lit (1 :: Int)) (Var "y")) (Var "z")
Add (Lit 1) (Var "z")

Test> close "y" (Add (Lit (1 :: Int)) (Var "y")) (Lit 7)
Add (Lit 1) (Lit 7)

-}

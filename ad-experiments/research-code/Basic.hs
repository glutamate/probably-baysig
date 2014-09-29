{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

-- What needs to be done here is to differentiate a captured function.  This is
-- proving to be difficult because the type has to be Num a => a -> a or
-- [a] -> a.

module Basic where

import Utils
import System.IO.Unsafe

-- | Language expressions.
data Expr = 
    Lit Int
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  deriving (Eq, Ord, Show, Data, Typeable)

instance MuRef Expr where
  type DeRef Expr        = ExprF
  mapDeRef f (Add e0 e1) = AddF <$> f e0 <*> f e1
  mapDeRef f (Sub e0 e1) = SubF <$> f e0 <*> f e1
  mapDeRef f (Mul e0 e1) = MulF <$> f e0 <*> f e1
  mapDeRef _ (Lit v)     = pure (LitF v)
  mapDeRef _ (Var s)     = pure (VarF s)

instance Num Expr where
  fromInteger = Lit . fromInteger
  e0 + e1     = Add e0 e1
  e0 - e1     = Sub e0 e1
  e0 * e1     = Mul e0 e1
  signum      = undefined
  abs         = undefined

-- | Pattern functor corresponding to 'Expr'.
data ExprF e =
    LitF Int
  | AddF e e
  | SubF e e
  | MulF e e
  | VarF String
  deriving (Eq, Ord, Show, Functor)

-- | Evaluate expressions.
eval :: Expr -> [(String, Int)] -> Int
eval (Lit d)     _   = d
eval (Add e0 e1) env = eval e0 env + eval e1 env
eval (Sub e0 e1) env = eval e0 env - eval e1 env
eval (Mul e0 e1) env = eval e0 env * eval e1 env
eval (Var v) env     = fromJust $ lookup v env

-- | Tree builder.
--
--   > tree 30
--   1073741824
--
--   > eval (tree 30) []
--   -- don't wait up
--
tree :: (Num a, Eq a, Num b) => a -> b
tree 0 = 1
tree n =
  let shared = tree (n - 1)
  in  shared + shared

autoTree :: (Num b, Eq b, Mode t, Scalar t ~ Expr) => b -> t
autoTree 0 = auto 1
autoTree n = 
  let shared = tree (n - 1)
  in  auto $ shared + shared

-- | Cast an expression to a function.  Assumes one free variable, which must
--   be passed in string form as an argument.
toFunction :: Expr -> String -> (Int -> Int)
toFunction e x = \l -> eval (capture e x l) []

-- | Cast an expression to a function.  All free variables and their
--   replacement values must be held in the environment passed as an argument.
toFunctionWithEnv :: Expr -> ([(String, Int)] -> Int)
toFunctionWithEnv e = \ls -> eval (captureWithEnv e ls) ls 

-- | Capture the body of a function, replacing the named free variable with the
--   provided value.
capture :: Expr -> String -> Int -> Expr
capture e x l = transform replaceVar e where
  replaceVar (Var y) | y == x = Lit l
  replaceVar y = y

-- | Capture the body of a function, replacing any named free variables with
--   their provided values.
captureWithEnv :: Expr -> [(String, Int)] -> Expr
captureWithEnv e ls = transform replaceVar e where
  replaceVar (Var y) = Lit . fromJust . lookup y $ ls
  replaceVar y = y

-- | Evaluate a graph that is assumed to include no free variables.
evalGraph :: Graph ExprF -> Int
evalGraph (Graph env r) = go r where
  go j = case lookup j env of
    Just (MulF a b) -> go a * go b
    Just (AddF a b) -> go a + go b
    Just (SubF a b) -> go a - go b
    Just (VarF _)   -> error "evalGraph: contains free variable"
    Just (LitF d)   -> d
    Nothing         -> 0

-- | Evaluate a graph by passing an environment containing values for free
--   variables.
evalGraphWithEnv :: Graph ExprF -> [(String, Int)] -> Int
evalGraphWithEnv (Graph env r) e = go r where
  go j = case lookup j env of
    Just (MulF a b) -> go a * go b
    Just (AddF a b) -> go a + go b
    Just (SubF a b) -> go a - go b
    Just (VarF v)   -> fromJust $ lookup v e
    Just (LitF d)   -> d
    Nothing         -> 0

test :: Expr
test = Var "x" ^ 10

testo :: Int -> Expr
testo = capture test "x"

main :: IO ()
main = do
  -- presumably we would use unsafePerformIO in practice
  let g0 = unsafePerformIO . reifyGraph $ capture test "x" 2
      g1 = cse g0

  print $ evalGraph g0
  print $ evalGraph g1

closed (Lit _) = True
closed (Var _) = False
closed (Add e0 e1) = closed e0 && closed e1
closed (Sub e0 e1) = closed e0 && closed e1
closed (Mul e0 e1) = closed e0 && closed e1

evalHard (Lit d) = d
evalHard (Var _) = error "Bang"
evalHard (Add e0 e1) = evalHard e0 + evalHard e1
evalHard (Sub e0 e1) = evalHard e0 - evalHard e1
evalHard (Mul e0 e1) = evalHard e0 * evalHard e1

-- check if closed, then eval that
evalALittle (Add e0 e1)
  | closed e0 && closed e1       = Lit (evalHard e0 + evalHard e1)
  | closed e0 && not (closed e1) = Add (Lit $ evalHard e0) (evalALittle e1)
  | not (closed e0) && closed e1 = Add (evalALittle e0) (Lit $ evalHard e1)
  | otherwise = Add (evalALittle e0) (evalALittle e1)

evalALittle (Sub e0 e1)
  | closed e0 && closed e1       = Lit (evalHard e0 - evalHard e1)
  | closed e0 && not (closed e1) = Sub (Lit $ evalHard e0) (evalALittle e1)
  | not (closed e0) && closed e1 = Sub (evalALittle e0) (Lit $ evalHard e1)
  | otherwise = Sub (evalALittle e0) (evalALittle e1)

evalALittle (Mul e0 e1)
  | closed e0 && closed e1       = Lit (evalHard e0 * evalHard e1)
  | closed e0 && not (closed e1) = Mul (Lit $ evalHard e0) (evalALittle e1)
  | not (closed e0) && closed e1 = Mul (evalALittle e0) (Lit $ evalHard e1)
  | otherwise = Mul (evalALittle e0) (evalALittle e1)

evalALittle x = x



altEval x expr = (`go` expr) where
  go _ (Lit d) = auto d
  go v (Var s)
    | s == x    = v
    | otherwise = error "expression not closed"

  go v (Add e0 e1) = go v e0 + go v e1
  go v (Sub e0 e1) = go v e0 - go v e1
  go v (Mul e0 e1) = go v e0 * go v e1

-- toDerivative v expr = diff (altEval v expr) (Var "x")


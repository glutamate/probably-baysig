-- Forward-mode AD in a very simple symbolic language.
--
-- Is there any problem with the implementation for this language?  It
-- differentiates symbolic expressions; but can it return symbolic functions?
-- That's the goal.
--
-- Right now if I do 'embed example' I get a function back but I can't observe
-- it.  I think that might be what I need here - the 'observable application'
-- as is described in the Nikola paper.
--
-- So what I need to do next is reify something like:
--
-- >> \f x -> evalDual $ embed f
--
-- As a side problem, there is no sharing here.
--

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

data Expr =
    Lit Double
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  deriving (Eq, Show)

instance Num Expr where
  fromInteger = Lit . fromInteger
  e0 + e1     = Add e0 e1
  e0 - e1     = Sub e0 e1
  e0 * e1     = Mul e0 e1
  signum      = undefined
  abs         = undefined

data Dual = Dual {
    primal  :: Expr
  , tangent :: Expr
  } deriving (Eq, Show)

instance Num Dual where
  fromInteger                 = constD . fromInteger
  (Dual e0 e1) + (Dual y0 y1) = Dual (e0 + y0) (e1 + y1)
  (Dual e0 e1) * (Dual y0 y1) = Dual (e0 * y0) (e0 * y1 + y0 * e1)
  negate (Dual e0 e1)         = Dual (negate e0) (negate e1)
  signum _                    = undefined
  abs _                       = undefined

eval :: Expr -> Double
eval (Lit j)     = j
eval (Add e0 e1) = eval e0 + eval e1
eval (Sub e0 e1) = eval e0 - eval e1
eval (Mul e0 e1) = eval e0 * eval e1

text :: Expr -> String
text (Lit j)     = show j
text (Add e0 e1) = "(" ++ text e0 ++ " + " ++ text e1 ++ ")"
text (Sub e0 e1) = "(" ++ text e0 ++ " - " ++ text e1 ++ ")"
text (Mul e0 e1) = "(" ++ text e0 ++ " * " ++ text e1 ++ ")"

constD :: Expr -> Dual
constD x = Dual x (Lit 0)

idD :: Expr -> Dual
idD x = Dual x (Lit 1.0)

evalDual :: Dual -> Dual 
evalDual (Dual e0 e1) = Dual (Lit $ eval e0) (Lit $ eval e1)

embed :: (Dual -> c) -> Expr -> c
embed f = f . idD

example :: Num a => a -> a
example x = x ^ 2 + 2 * x + 1

square :: Num a => a -> a
square x = x * x

tree :: (Num a, Eq a) => a -> Expr
tree 0 = Lit 1
tree n =
  let shared = tree (n - 1)
  in  shared + shared

main :: IO ()
main = do
  let g = embed example
  print . g $ 3
  print . evalDual . g $ 3


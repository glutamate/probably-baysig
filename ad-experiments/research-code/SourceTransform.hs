
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module SourceTransform where

import Data.Data
import Data.Generics.Uniplate.Data
import Utils

data Expr = 
    Lit Int
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  deriving (Eq, Ord, Show, Data, Typeable)

instance Num Expr where
  fromInteger = Lit . fromInteger
  e0 + e1     = Add e0 e1
  e0 - e1     = Sub e0 e1
  e0 * e1     = Mul e0 e1
  signum      = undefined
  abs         = undefined

text :: Expr -> String
text (Lit j)     = show j
text (Add e0 e1) = "(" ++ text e0 ++ " + " ++ text e1 ++ ")"
text (Sub e0 e1) = "(" ++ text e0 ++ " - " ++ text e1 ++ ")"
text (Mul e0 e1) = "(" ++ text e0 ++ " * " ++ text e1 ++ ")"
text (Var x)     = x

sharedSquare :: Num a => a -> a
sharedSquare x =
  let shared = x
  in  shared * shared

sharedSumSquares :: Num a => [a] -> a
sharedSumSquares [x, y] =
  let shared0 = sharedSquare x
      shared1 = sharedSquare y
  in  shared0 + shared1
sharedSumSquares _ = undefined

tree :: (Num a, Eq a, Num b) => a -> b
tree 0 = 1
tree n = let shared = tree (n - 1)
         in  shared + shared

testExpr = Var "x" * Var "x" * (1 + 2 + 3) + 4 * Var "x"

-- c * x ^ n = c n x ^ (n - 1)

variables :: Expr -> [String]
variables x = [y | Var y <- universe x]

mulsToAdds = transform mulToAdd where
  mulToAdd (Mul a b) = Add a b
  mulToAdd x         = x



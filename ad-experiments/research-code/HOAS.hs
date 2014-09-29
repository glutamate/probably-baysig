{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module HOAS where

import Data.Map (Map)
import qualified Data.Map as Map
import Utils

data Expr =
    Lit Int
  | Add Expr Expr
  | Let Expr (Expr -> Expr)

instance Num Expr where
  fromInteger = Lit . fromInteger
  (+)         = Add

eval :: Expr -> Int
eval (Lit d)     = d
eval (Add e0 e1) = eval e0 + eval e1
eval (Let e f) =
  let v = Lit (eval e)
  in  eval (f v)

text :: Expr -> String
text (Lit d)     = "Lit " ++ show d
text (Add e0 e1) = "Add (" ++ text e0 ++ ") (" ++ text e1 ++ ")"
text (Let e f)   = "Let " ++ eval f 


-- need parser such that print (parse x) = x


test :: Int -> Expr
test x = Let (Lit x) (\v -> Add v (Lit 1))

testo x = Let x (\x0 -> Let (Add (Lit 1) x0) (\x1 -> Add x1 x1))




{-# LANGUAGE FlexibleInstances #-}

module Math.Probably.FunBUGS where

import Data.List

data Model = Model [(String, Dist Double)] [(String, Expr)]

data Dist a = Beta a a
            | Exp a
            | Norm a a
            | Gamma a a

data Expr = Var String
          | M1 Op1 Expr
          | M2 Op2 Expr Expr
          | If Expr Expr Expr
          | Cmp CmpOp Expr Expr
          | Dist (Dist Expr)

(<~) = (,)

data Op1 = FExp | FLn
data Op2 = Add | Sub | Mul | Div
data CmpOp = Lt | Gt | Eq

class RDump a where
    rdump :: String -> a -> String

instance Show a => RDump [a] where
    rdump nm xs = nm++" <-\nc("++(intercalate ", " $ map show xs)++")\n"

regressModel = Model
                  ["alpha" <~ Norm 0 0.0001,
                   "beta" <~ Norm 0 0.0001]
                  []
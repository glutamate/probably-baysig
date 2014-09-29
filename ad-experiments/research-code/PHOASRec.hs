{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}

module PHOASRec where

import Control.Applicative
import Data.Function
import Data.Maybe
import Utils

data Expr a =
    Lit Int
  | Var a
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Lam (a -> Expr a)
  | App (Expr a) (Expr a)
  | Let (Expr a) (a -> Expr a)
  | Mu (a -> Expr a)
  | LetRec ([a] -> [Expr a]) ([a] -> Expr a)

instance Num (Expr a) where
  fromInteger = Lit . fromInteger
  e0 + e1     = Add e0 e1
  e0 - e1     = Sub e0 e1
  e0 * e1     = Mul e0 e1
  signum      = undefined
  abs         = undefined

instance MuRef (Expr a) where
  type DeRef (Expr a) = ExprF
  mapDeRef _ (Lit i)     = pure (LitF i)
  mapDeRef f (Add e0 e1) = AddF <$> f e0 <*> f e1

instance MuRef ExprD where
  type DeRef ExprD        = ExprF
  mapDeRef _ (LitD i)     = pure (LitF i)
  mapDeRef f (AddD e0 e1) = AddF <$> f e0 <*> f e1

type ClosedExpr = forall a. Expr a

text :: Expr String -> String
text e = go e 0 where
  go (Lit j)     _ = "Lit " ++ show j
  go (Add e0 e1) c = "Add (" ++ go e0 c ++ ") (" ++ go e1 c ++ ")"
  go (Sub e0 e1) c = "Sub (" ++ go e0 c ++ ") (" ++ go e1 c ++ ")"
  go (Mul e0 e1) c = "Mul (" ++ go e0 c ++ ") (" ++ go e1 c ++ ")"
  go (Var x) _     = x
  go (Let e0 e1) c = "(Let " ++ v ++ " (" ++ go e0 (c + 1) ++
                     ") (" ++ go (e1 v) (c + 1) ++ "))"
    where v = "v" ++ show c

instance Show (Expr String) where
  show = text

data ExprD =
    LitD Int
  | AddD ExprD ExprD
  deriving Show

data ExprF r =
    LitF Int
  | AddF r r
  deriving Show

data Value =
    N Int
  | F (Value -> Value)

instance Show Value where
  show (N d) = show d
  show (F _) = "<function>"

eval :: Expr Value -> Value
eval (Lit i)       = N i
eval (Add e0 e1)   = add (eval e0) (eval e1)
eval (Sub e0 e1)   = sub (eval e0) (eval e1)
eval (Mul e0 e1)   = mul (eval e0) (eval e1)
eval (Var v)       = v
eval (Let e0 e1)   = eval (e1 (eval e0))
eval (Lam e)       = F (\v -> eval (e v))
eval (App e0 e1)   = app (eval e0) (eval e1)
eval (Mu e)        = fix (\v -> eval (e v))
eval (LetRec es e) = eval (e (fix (map eval . es)))

let_ :: Expr a -> (Expr a -> Expr a) -> Expr a
let_ e0 e1 = Let e0 (\x -> e1 (Var x))

lam_ :: (Expr a -> Expr a) -> Expr a
lam_ e = Lam (\x -> e (Var x))

mu_ e = Mu (\x -> e (Var x))

letrec_ es e = LetRec (es . map Var) (e . map Var)

add (N m) (N n) = N (m + n)
sub (N m) (N n) = N (m - n)
mul (N m) (N n) = N (m * n)

app (F f) v = f v

one = LitD 1
  
build :: [(Int, ExprF Int)] -> Int -> ClosedExpr
build env root =
    letrec_
      (\vs ->
        let go (LitF x)     = Lit x
            go (AddF v0 v1) = Add (var vs v0) (var vs v1)
        in  map (go . snd) env)
      (\vs -> var vs root)
  where
    var vs n = fromJust (lookup n (zipWith (\(i,_) x -> (i, x)) env vs))

treeD 0 = LitD 1
treeD n = let shared = treeD (n - 1) in AddD shared shared
    
treeI :: (Num a, Eq a) => a -> Expr String
treeI 0 = Lit 1
treeI n = let shared = treeI (n - 1) in Add shared shared

treeE :: (Num a, Eq a) => a -> Expr String
treeE 0 = Lit 1
treeE n = let_ (treeE (n - 1)) (\shared -> shared + shared)

-- test = do
--   (Graph env r) <- reifyGraph (treeI 3)
--   print (text (build env r))






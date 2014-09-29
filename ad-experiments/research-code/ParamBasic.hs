{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module ParamBasic where

import Control.Applicative
import Control.Monad
import Data.Data
import Data.Generics.Uniplate.Data
import Data.Graph
import Data.Maybe
import Data.Reify hiding (Graph)
import qualified Data.Reify as Reify
import Data.Reify.Graph.CSE
import Numeric.AD
import System.IO.Unsafe
import System.Random.MWC

data Expr a =
    Lit a
  | Var String
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  deriving (Eq, Show, Data, Typeable)

instance Variate a => Variate (Expr a) where
  uniform  g                = liftM Lit $ uniform g
  uniformR (Lit a, Lit b) g = liftM Lit $ uniformR (a, b) g
  uniformR _              _ = error "unimplemented"

instance Num a => Num (Expr a) where
  fromInteger = Lit . fromInteger
  e0 + e1     = Add e0 e1
  e0 - e1     = Sub e0 e1
  e0 * e1     = Mul e0 e1

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

autoEval :: Mode t => String -> Expr (Scalar t) -> t -> t
autoEval x e0 = (`ec` e0) where
  ec _ (Lit d) = auto d
  ec v (Var y) 
    | x == y    = v
    | otherwise = error "kaboom"
  ec v (Add e1 e2) = ec v e1 + ec v e2
  ec v (Sub e1 e2) = ec v e1 - ec v e2
  ec v (Mul e1 e2) = ec v e1 * ec v e2

-- an optimized derivative?  or just completely circular?
--
-- what i probably want is the reified AST corresponding to this function 
diff_
  :: (Mode c, MuRef a, Ord (Scalar c), Num a, DeRef a ~ ExprF (Scalar c))
  => Expr a -> String -> a -> c
diff_ expr x = evalGraph . cse . toGraphDerivative expr x

toGraphDerivative
  :: (MuRef s, Num s, Ord (DeRef s Unique), Functor (DeRef s))
  => Expr s -> String -> s -> Reify.Graph (DeRef s)
toGraphDerivative e x v =
  cse . unsafePerformIO . reifyGraph $ diff (autoEval x e) v

evalGraph g = consume $ toGraph g where
  toGraph (Reify.Graph env _) = graphFromEdges . map toNode $ env
  toNode (j, AddF a b) = (AddF a b, j, [a, b])
  toNode (j, SubF a b) = (SubF a b, j, [a, b])
  toNode (j, MulF a b) = (MulF a b, j, [a, b])
  toNode (j, LitF d)   = (LitF d, j, [])
  toNode (_, VarF _)   = error "kaboom!"


-- graphEval :: Num a => Expr a -> a
graphEval expr = consume reified where
  reified = unsafePerformIO (toGraph <$> reifyGraph expr)
  toGraph (Reify.Graph env _) = graphFromEdges . map toNode $ env
  toNode (j, AddF a b) = (AddF a b, j, [a, b])
  toNode (j, SubF a b) = (SubF a b, j, [a, b])
  toNode (j, MulF a b) = (MulF a b, j, [a, b])
  toNode (j, LitF d)   = (LitF d, j, [])
  toNode (_, VarF _)   = error "kaboom!"

-- consume :: (Num d, Eq a) => (Graph, Vertex -> (ExprF d a, a, b), c) -> d
consume (g, vmap, _) = go (reverse . topSort $ g) [] where
  go [] acc = snd $ head acc
  go (v:vs) acc =
    let nacc = evalNode (vmap v) acc : acc
    in  go vs nacc

-- need to incorporate the variable name in evalNode
-- evalNode :: (Num d, Eq a) => (ExprF d a, b, c) -> [(a, d)] -> (b, d)
evalNode (LitF d, k, _)   _ = (k, auto d)

evalNode (VarF _, _, _)   _ = error "kaboom!"

evalNode (AddF a b, k, _) l =
  let v = fromJust ((+) <$> lookup a l <*> lookup b l)
  in  (k, v)

evalNode (SubF a b, k, _) l =
  let v = fromJust ((-) <$> lookup a l <*> lookup b l)
  in  (k, v)

evalNode (MulF a b, k, _) l =
  let v = fromJust ((*) <$> lookup a l <*> lookup b l)
  in  (k, v)

-- so, our expression:
test :: Num a => Expr a
test = Var "x" ^ 100


-- examples:
-- toGraphDerivative test "x" (Lit 10)
-- cse $ toGraphDerivative test "x" (Lit 10)

-- evalClose :: Mode t => String -> Expr (Scalar t) -> t -> t
-- evalClose x e0 = (`ec` e0) where
--   ec _ (Lit d) = auto d
--   ec v (Var y) 
--     | x == y    = v
--     | otherwise = error "kaboom"
--   ec v (Add e1 e2) = ec v e1 + ec v e2
--   ec v (Sub e1 e2) = ec v e1 - ec v e2
--   ec v (Mul e1 e2) = ec v e1 * ec v e2

obs :: (Num a, Variate a) => [Expr a]
obs = unsafePerformIO $ do
  g  <- create
  vs <- replicateM 200 (uniform g)
  return $! vs

benchExp :: (Num a, Variate a) => Expr a
benchExp = sum (map (^ 2) $ zipWith subtract obs (replicate l (Var "x"))) where
  l = length (obs :: [Expr Double])


eval (Lit d) = d
eval (Var _) = error "expression not closed"
eval (Add e0 e1) = eval e0 + eval e1
eval (Sub e0 e1) = eval e0 - eval e1
eval (Mul e0 e1) = eval e0 * eval e1

-- | Close an expression over a variable.
close s x (Add e0 e1) = Add (close s x e0) (close s x e1)
close s x (Sub e0 e1) = Sub (close s x e0) (close s x e1)
close s x (Mul e0 e1) = Mul (close s x e0) (close s x e1)

close s x (Var v)
  | v == s    = Lit x
  | otherwise = Var v

close _ _ e = e

-- Reify a derivative
altEval :: Mode a => String -> Expr (Scalar a) -> a -> a
altEval x expr = (`go` expr) where
  go _ (Lit d) = auto d
  go v (Var s)
    | s == x    = v
    | otherwise = error "expression not closed"

  go v (Add e0 e1) = go v e0 + go v e1
  go v (Sub e0 e1) = go v e0 - go v e1
  go v (Mul e0 e1) = go v e0 * go v e1

aEval expr = go expr where
  go (Lit d) = auto d
  go (Var _) = error "bang"
  go (Add e0 e1) = go e0 + go e1
  go (Sub e0 e1) = go e0 - go e1
  go (Mul e0 e1) = go e0 * go e1

toDerivative v expr = diff (altEval v expr) (Var "x")

altDerivative expr x = eval . close "x" x $ diffExpr where
  diffExpr = toDerivative "x" expr


-- | Reduce superfluous expressions.
elimIdent :: (Num a, Eq a) => Expr a -> Expr a
elimIdent (Add (Lit 0) e) = elimIdent e
elimIdent (Add e (Lit 0)) = elimIdent e
elimIdent (Add e0 e1)     = Add (elimIdent e0) (elimIdent e1)

elimIdent (Sub (Lit 0) e) = elimIdent e
elimIdent (Sub e (Lit 0)) = elimIdent e
elimIdent (Sub e0 e1)     = Sub (elimIdent e0) (elimIdent e1)

elimIdent (Mul (Lit 1) e) = elimIdent e
elimIdent (Mul e (Lit 1)) = elimIdent e
elimIdent (Mul e0 e1)     = Mul (elimIdent e0) (elimIdent e1)

elimIdent e = e

testExpr :: Num a => Expr a
testExpr = 2 * Var "x" ^ 2 + Var "x" ^ 4

altEvalGraph s x g = consume $ toGraph g where
  toGraph (Reify.Graph env _) = graphFromEdges . map toNode $ env
  toNode (j, AddF a b) = (AddF a b, j, [a, b])
  toNode (j, SubF a b) = (SubF a b, j, [a, b])
  toNode (j, MulF a b) = (MulF a b, j, [a, b])
  toNode (j, LitF d)   = (LitF d, j, [])
  toNode (j, VarF v)
    | s == v = (LitF x, j, [])
    | otherwise = error "kaboom!"





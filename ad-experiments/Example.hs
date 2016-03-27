{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Example where

import Control.Applicative
import Data.Graph
import Data.Maybe
import qualified Data.Reify (Graph(..))
import Data.Reify hiding (Graph)
import Data.Reify.Graph.CSE
import Numeric.AD
import System.IO.Unsafe

-- | A demo language for illustrating this technique.  From what I can tell,
--   the minimum requirements are:
--
--   - it must be parameterized
--   - it must be an instance of Functor (see above)
--   - it must be an instance of Num
--
--   To recover sharing (for optimizing expressions), this particular language
--   also requires
--
--   - a corresponding pattern functor (ExprF)
--   - a MuRef instance that maps Expr to ExprF
--
data Expr a =
    Lit a
  | Var String
  | Add (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  deriving (Eq, Show, Functor)

instance Num a => Num (Expr a) where
  fromInteger = Lit . fromInteger
  (+) = Add
  (*) = Mul

-- | A pattern functor and associated MuRef instance.  See the 'data-reify'
--   package and 'Type-Safe Observable Sharing in Haskell':
--
--   http://www.cs.uu.nl/wiki/pub/Afp/CourseLiterature/Gill-09-TypeSafeReification.pdf
data ExprF a e =
    LitF a
  | VarF String
  | AddF e e
  | MulF e e
  deriving (Eq, Ord, Show, Functor)

instance MuRef (Expr a) where
  type DeRef (Expr a) = ExprF a
  mapDeRef f (Add e0 e1) = AddF <$> f e0 <*> f e1
  mapDeRef f (Mul e0 e1) = MulF <$> f e0 <*> f e1
  mapDeRef _ (Lit v)     = pure $ LitF v
  mapDeRef _ (Var v)     = pure $ VarF v

-- | Close an expression over some variable.
--
--   >>> close (Var "x") "x" 1
--   Lit 1
close :: Expr a -> String -> a -> Expr a
close (Add e0 e1) s x = Add (close e0 s x) (close e1 s x)
close (Mul e0 e1) s x = Mul (close e0 s x) (close e1 s x)
close (Var v) s x
  | v == s    = Lit x
  | otherwise = Var v

close e _ _ = e

-- | Evaluate an expression.
eval :: Num a => Expr a -> a
eval (Lit x)     = x
eval (Add e0 e1) = eval e0 + eval e1
eval (Mul e0 e1) = eval e0 * eval e1
eval Var {}      = error "eval: stuck"

-- | Evaluate an expression with lifted literals over a particular variable.
autoEval :: Mode a => String -> Expr (Scalar a) -> a -> a
autoEval x = go where
  go (Lit d) _     = auto d
  go (Add e0 e1) v = go e0 v + go e1 v
  go (Mul e0 e1) v = go e0 v * go e1 v
  go (Var s) v
    | s == x    = v
    | otherwise = error "incorrect variable"

-- | Differentiate an expression with respect to a named variable.
--
--   >>> toDerivative "x" (Mul (Lit 2) (Mul (Var "x") (Var "x")))
--   Mul (Add (Mul (Var "x") (Lit 1)) (Mul (Lit 1) (Var "x"))) (Lit 2)
toDerivative :: Num a => String -> Expr (Expr a) -> Expr a
toDerivative v expr = diff (autoEval v expr) (Var v)

-- | Optimize an expression by eliminating superfluous nodes
--   (language-specific).
elimIdent :: (Num a, Eq a) => Expr a -> Expr a
elimIdent (Add (Lit 0) e) = elimIdent e
elimIdent (Add e (Lit 0)) = elimIdent e
elimIdent (Add e0 e1)     = Add (elimIdent e0) (elimIdent e1)

elimIdent (Mul (Lit 1) e) = elimIdent e
elimIdent (Mul e (Lit 1)) = elimIdent e
elimIdent (Mul e0 e1)     = Mul (elimIdent e0) (elimIdent e1)

elimIdent e = e

-- | Example expression optimizer.  Applying a domain-specific optimization,
--   recover sharing, and then eliminate common subexpressions.
--
--   Converts the expression into a graph.
optimize :: (Ord a, Num a) => Expr a -> Data.Reify.Graph (ExprF a)
optimize = cse . unsafePerformIO . reifyGraph . elimIdent

-- | An example univariate graph evaluator, for evaluating optimized
--   expressions.
graphEval :: (Num a, Ord a) => String -> a -> Data.Reify.Graph (ExprF a) -> a
graphEval s x = consume . toGraph where
  toGraph (Data.Reify.Graph env _) = graphFromEdges $ toNode <$> env where
    toNode (j, AddF a b) = (AddF a b, j, [a, b])
    toNode (j, MulF a b) = (MulF a b, j, [a, b])
    toNode (j, LitF d)   = (LitF d, j, [])
    toNode (j, VarF v)   = (VarF v, j, [])

  consume (g, vmap, _) = go (reverse . topSort $ g) [] where
    go [] acc     = snd . head $ acc
    go (v:vs) acc = go vs nacc where
      nacc = evalNode (vmap v) acc : acc

  evalNode (LitF d, k, _) _   = (k, d)
  evalNode (AddF a b, k, _) l = (k, v) where
    v = fromJust $ liftA2 (+) (lookup a l) (lookup b l)
  evalNode (MulF a b, k, _) l = (k, v) where
    v = fromJust $ liftA2 (*) (lookup a l) (lookup b l)
  evalNode (VarF v , k, _) _
    | v == s    = (k, x)
    | otherwise = error "evalNode: stuck"

-- | Example
main :: IO ()
main = do
  let ex        = Mul (Lit 3) (Mul (Var "x") (Var "x"))
      diffExpr  = toDerivative "x" ex
      optimized = optimize diffExpr

  putStrLn "example expression:"
  print ex

  putStrLn ""

  putStrLn "differentiated expression:"
  print diffExpr

  putStrLn ""

  putStrLn "optimized differentiated expression:"
  print optimized

  putStrLn ""

  putStrLn "result:"
  print $ graphEval "x" 1 optimized

  return ()


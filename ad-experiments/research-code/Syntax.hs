-- data Expr a =
--     Lit Double
--   | Add (Expr a) (Expr a)
--   | Sub (Expr a) (Expr a)
--   | Mul (Expr a) (Expr a)
--   deriving (Eq, Show)
-- 
-- instance Num (Expr a) where
--   fromInteger = Lit . fromInteger
--   e0 + e1     = Add e0 e1
--   e0 - e1     = Sub e0 e1
--   e0 * e1     = Mul e0 e1
--   signum      = undefined
--   abs         = undefined

-- eval (Var n)     = n
-- eval (Let e0 e1) = eval (e1 (eval e0))

-- textE :: ClosedExpr -> String
-- textE e = go e 0 where
--   go (Lit j)     _ = show j
--   go (Add e0 e1) c = "(" ++ go e0 c ++ " + " ++ go e1 c ++ ")"
--   go (Sub e0 e1) c = "(" ++ go e0 c ++ " - " ++ go e1 c ++ ")"
--   go (Mul e0 e1) c = "(" ++ go e0 c ++ " * " ++ go e1 c ++ ")"
--   go (Var x) _     = x
--   go (Let e0 e1) c = "(let " ++ v ++ " " ++ go e0 (c + 1) ++
--                      " in " ++ go (e1 v) (c + 1) ++ ")"
--     where v = "v" ++ show c

-- text :: Expr a -> String
-- text (Lit j)     = show j
-- text (Add e0 e1) = "(" ++ text e0 ++ " + " ++ text e1 ++ ")"
-- text (Sub e0 e1) = "(" ++ text e0 ++ " - " ++ text e1 ++ ")"
-- text (Mul e0 e1) = "(" ++ text e0 ++ " * " ++ text e1 ++ ")"

-- data Dual a = Dual (Expr a) (Expr a) deriving (Eq, Show)


-- instance Num (Dual a) where
--   fromInteger                 = constD . fromInteger
--   (Dual e0 e1) + (Dual y0 y1) = Dual (e0 + y0) (e1 + y1)
--   (Dual e0 e1) * (Dual y0 y1) = Dual (e0 * y0) (e0 * y1 + y0 * e1)
--   negate (Dual e0 e1)         = Dual (negate e0) (negate e1)
--   signum _                    = undefined
--   abs _                       = undefined

constD x = Dual x (Lit 0)
idD x    = Dual x (Lit 1.0)


expr = constD 1 + constD 2 * constD 3

treeE 0 = constD 1
treeE n = let shared = treeE (n - 1) in shared + shared

f x = 2 * x * x + 1
g x = let Dual _ (Lit d) = optimize $ f (idD x)
      in  d





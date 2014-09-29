The 'research-code' subdirectory contains a bunch of experimental code
pertaining to implementing symbolic differentiation by way of automatic
differentiation.  The main results are summarized in Example.hs in the
ad-experiments directory.  For supplementary reading, see

http://jtobin.ca/blog/2014/07/06/automasymbolic-differentiation/

The crux of this research was to examine how one could implement symbolic
differentiation using automatic differentiation.  The goal is to avoid
doing AD more than is necessary in an iterative gradient-based algorithm, such
as is required in some optimization or sampling methods.  Instead of running
forward- or reverse-mode AD at every iteration of an algorithm, we would
prefer to instead use AD *once* and recover a cheap, generic function that then
be used repeatedly for future iterations.

Using Haskell's de-facto standard automatic differentiation library 'ad', it
appears to be possible to do this for any expression type that can be made an
instance of both the Functor and Num typeclasses.  The fundamental idea turns
out to be simple: provide a separate evaluation function that 'lifts' language
literals into the 'ad' world using the `auto` combinator, i.e.:

    -- | Evaluate an expression with lifted literals over a particular variable.
    autoEval :: Mode a => String -> Expr (Scalar a) -> a -> a
    autoEval x = go where
      go (Lit d) _     = auto d
      go (Add e0 e1) v = go e0 v + go e1 v
      go (Mul e0 e1) v = go e0 v * go e1 v
      go (Var s) v
        | s == x    = v
        | otherwise = error "incorrect variable"

That eval function can then be used to reify a differentiated expression in
terms of abstract syntax:

    -- | Differentiate an expression with respect to a named variable.
    toDerivative :: Num a => String -> Expr (Expr a) -> Expr a
    toDerivative v expr = diff (autoEval v expr) (Var v)

The recovered symbolic expression for the derivative can then be optimized in a
number of ways - most of which depend on the abstractions present in the
underlying language.  For example, if the underlying language has arithmetic
primitives like 'Add', 'Sub', 'Div', or 'Mul', then it's possible to reduce
terms such as `Add (Lit 1) (Lit 0)` to simply `Lit 1`, and so on.  A generic,
low-hanging optimization involves recovering implicit sharing using the
'data-reify' library, and performing common subexpression elimination on the
syntax graphs it produces.

To evaluate the stored abstract syntax, just evaluate it with an appropriate
eval function for a given variable at the desired point.

The examples here focus on univariate functions, but an extension to gradients
seems straightforward; instead of passing a variable name and point, one can
pass an environment.


{-# language LambdaCase #-}
module PPL.Internal.McBride04 where

{-
Functional Pearl: I am not a Number—I am a Free Variable
C.McBride, J.McKinna, 2004
http://www.cs.ru.nl/~james/RESEARCH/haskell2004.pdf
-}

data Expr v =
    F v               -- ^ Free variables
  | B Int             -- ^ Bound variables
  | Expr v :$ Expr v  -- ^ Application
  | Expr v :-> Expr v -- ^ Lambda abstraction
  deriving (Eq, Show)

-- | Expr is the type of closed expressions — those with no ‘dangling’ bound variables pointing out of scope, and Scope has one dangling bound variable, called B 0 at the top level
newtype Scope v = Sc (Expr v) deriving (Eq, Show)


abstract :: Eq v => v -> Expr v -> Scope v
abstract name expr = Sc (nameTo 0 expr) where
  nameTo outer = \case
    F name' | name == name' -> B outer
            | otherwise -> F name'
    B index -> B index
    fun :$ arg -> nameTo outer fun :$ nameTo outer arg
    dom :-> body -> nameTo outer dom :-> nameTo (outer + 1) body


instantiate :: Expr v -> Scope v -> Expr v
instantiate image (Sc body) = replace 0 body where
  replace outer = \case
    B index | index == outer -> image
            | otherwise -> B index
    F name -> F name
    fun :$ arg -> replace outer fun :$ replace outer arg
    dom :-> bod -> replace outer dom :-> replace (outer + 1) bod

subst :: Eq v => Expr v -> v -> Expr v -> Expr v
subst img name = instantiate img . abstract name    



--

ex0 = F "moo" :$ (B 0 :-> B 0)

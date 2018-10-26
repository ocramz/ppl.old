module Lib where

-- data Expr a =
--     Const a
--   | Var String

type Var = String

data Expr a =
    Const a
  | Let (Var, Expr a) (Expr a)
  deriving (Eq, Show)


lets :: Foldable t => t (Var, Expr a) -> Expr a -> Expr a
lets es e0 = foldr Let e0 es

-- eval e = case e of
--   Const x -> x
  



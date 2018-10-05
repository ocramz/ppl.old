module Lib where

-- data Expr a =
--     Const a
--   | Var String

type Var = String

data Expr a =
    Const a
  | Let Var (Expr a)
  deriving (Eq, Show)


eval e = case e of
  Const x -> x
  



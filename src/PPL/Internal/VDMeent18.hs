{-# language LambdaCase #-}
module PPL.Internal.VDMeent18 where

import qualified Data.IntMap as IM
import qualified PPL.Internal.IEnv as IE

data Expr v a =
    Const a
  | Var v
  -- | Let
  -- | Lam
  | If Bool (Expr v a) (Expr v a)
  | Add (Expr v a) (Expr v a)
  -- | Mul (Expr v a) (Expr v a)   -- ...
  deriving (Eq, Show)

-- | 'defn f x = ...' == let f (lambda x .  ...)


-- | Boolean expressions (i.e. that evaluate to a Boolean value)
data B v a =
    GeT (Expr v a) (Expr v a)
  | LeT (Expr v a) (Expr v a)
  | Eql (Expr v a) (Expr v a)
  deriving (Eq, Show)

-- interpB env = \case
--   GeT e1 e2 -> do
--     a <- interpB env e1
--     b <- interpB env e2
--     pure (a >= b)
    

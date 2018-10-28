{-# language LambdaCase #-}
module PPL.Internal.Mini where

import Control.Monad.Catch (MonadThrow(..), throwM)
import GHC.Exception
import Data.Typeable


-- Pierce, Ch. 7

data Expr i a =
    Var i -- ^ Variable
  | Lam (Expr i a)  -- ^ Lambda
  | Expr i a :$ Expr i a  -- ^ Application
  deriving (Eq, Show)

termShift :: (Ord i, Num i) => i -> Expr i a1 -> Expr i a2
termShift d term = walk 0 term where
  walk c t0 = case t0 of
    Var x    -> if x >= c then Var (x + d) else Var x
    Lam t    -> Lam $ walk (c + 1) t
    t1 :$ t2 -> walk c t1 :$ walk c t2

termSubst :: (Num i, Ord i) => i -> Expr i a1 -> Expr i a2 -> Expr i a3
termSubst j s term = walk 0 term where
  walk c t0 = case t0 of
    Var x    -> if x == j + c then termShift c s else Var x
    Lam t    -> Lam $ walk (c + 1) t
    t1 :$ t2 -> walk c t1 :$ walk c t2

termSubstTop :: (Ord i, Num i) => Expr i a1 -> Expr i a4 -> Expr i a5
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)



-- Pierce, TAPL, Ch. 3-6

-- data V i =
--     B i  -- ^ Bound variables
--   | F i  -- ^ Free variables
--   deriving (Eq, Show)

-- mkVar :: V Integer
-- mkVar = F 0

-- data Expr i a =
--     Var (V i) -- ^ Variable
--   | Lam (Expr i a)  -- ^ Lambda
--   | Expr i a :$ Expr i a  -- ^ Application
--   deriving (Eq, Show)

-- -- reduce = \case
-- --   e1 :$ e2 -> \case
    



-- -- | Primitive operations
-- data OP = Plus | Times deriving (Eq, Show)

-- -- | Expression type
-- data Expr v a =
--     v :-> Expr v a       -- ^ Lambda
--   | Expr v a :$ Expr v a -- ^ Apply
--   -- | Prim OP (Expr v a) (Expr v a) -- ^ Primitive operation (e.g. '+')
--   | Const a
--   deriving (Eq, Show)

-- -- | 'Let' is equivalent to applying a lambda
-- let_ :: v -> Expr v a -> Expr v a -> Expr v a
-- let_ v e body = (v :-> body ) :$ e

-- lets_ :: Foldable t => t (v, Expr v a) -> Expr v a -> Expr v a
-- lets_ decls body = foldl (flip $ uncurry let_) body decls


-- -- | Value type
-- data Value m a =
--     Val a
--   | Fun (Value m a -> m (Value m a))

-- -- -- | Interpreting an 'Expr' produces a 'Value'
-- -- interp env = \case
-- --   Const x -> pure $ Val x
-- --   v :-> body -> pure $ Fun $ \v -> interp env' body  -- implement capture-avoiding subs
-- --   e1 :$ e2 -> do
-- --     f <- interp env e1
-- --     e <- interp env e2
-- --     apply f e 
-- --   where
-- --     apply v x = case v of
-- --       Fun f -> f x




-- * Exceptions

data Except = NotAFunction String deriving (Eq, Show, Typeable)
instance Exception Except

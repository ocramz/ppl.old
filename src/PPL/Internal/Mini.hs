{-# language LambdaCase #-}
module PPL.Internal.Mini where

import Control.Monad.Catch (MonadThrow(..), throwM)
import GHC.Exception
import Data.Typeable


data V i =
    B i  -- ^ Bound variables
  | F i  -- ^ Free variables
  deriving (Eq, Show)

mkVar :: V Integer
mkVar = F 0

data Expr i a =
    Var (V i) -- ^ Variable
  | Lam (Expr i a)  -- ^ Lambda
  | Expr i a :$ Expr i a  -- ^ Application
  deriving (Eq, Show)



-- reduce = \case
--   e1 :$ e2 -> \case
    



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

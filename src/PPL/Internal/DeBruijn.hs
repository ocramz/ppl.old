{-# language DeriveFunctor #-}
module PPL.Internal.DeBruijn where

import qualified Data.Map as M
import qualified Data.IntMap as IM

import Control.Monad.Catch (MonadThrow(..), throwM)
import GHC.Exception
import Data.Typeable

import Prelude hiding (lookup)

data Bind v =
    Zero
  | Succ v deriving (Eq, Show, Functor)

data Term v = Var v | App (Term v) (Term v) | Abs (Term (Bind v)) deriving (Eq, Show, Functor)

abstract :: Eq v => Term v -> v -> Term v
abstract t x = Abs $ lift t x

lift :: Eq t => Term t -> t -> Term (Bind t)
lift (Var y) x
  | y == x = Var Zero
  | otherwise = Var (Succ y)
lift (App u v) x = App (lift u x) (lift v x)
lift (Abs t) x = Abs $ lift t (Succ x)

reduce :: Term v -> Term v -> Term v
reduce (Abs s) t = subst s t

subst :: Term (Bind v) -> Term v -> Term v
subst (Var s) t = case s of
  Zero -> t
  Succ x -> Var x
subst (App u v) t = App (subst u t) (subst v t)
subst (Abs s) t = Abs $ subst s (Succ <$> t)







-- data Expr v a =
--     Var v
--   | Const a
--   | App (Expr v a) (Expr v a)
--   | Lam (Expr (Bind v) a) deriving (Eq, Show)

-- -- lift (Var y) x
-- --   | x == y = Var Zero
-- --   | otherwise = Var (Succ y)
-- -- lift (Lam t) x = Lam $ lift t (Succ x)  
-- lift (App u v) x = App (lift u x) (lift v x)  

-- data Value m a =
--     Val a
--   | Fun (Value m a -> m (Value m a))

-- -- interp :: Ord k =>
-- --           M.Map k (Value Maybe a)
-- --        -> Expr k a
-- --        -> Maybe (Value Maybe a)
-- interp env ex = case ex of
--   -- Lam 
--   App e1 e2 -> do
--     f <- interp env e1
--     e <- interp env e2
--     apply f e 
--   Const c -> pure $ Val c
--   Var x -> M.lookup x env

-- apply :: MonadThrow m => Value m a -> Value m a -> m (Value m a)
-- apply v a = case v of
--   Fun k -> k a
--   _ -> throwM $ NotAFunction "Not a function"
  
-- data Except = NotAFunction String deriving (Eq, Show, Typeable)
-- instance Exception Except











-- environment, encoded as IntMap

newtype Env a = Env { unEnv :: IM.IntMap a } deriving (Eq, Show)

empty :: Env a
empty = Env IM.empty

size :: Env a -> Int
size = IM.size . unEnv

mkEnv :: Foldable t => t a -> Env a
mkEnv xs = foldl (flip augment) empty xs

augment :: a -> Env a -> Env a
augment x (Env mm) = Env $ IM.insert k x mm where
  k = IM.size mm

lookup :: IM.Key -> Env a -> Maybe a
lookup v (Env mm) = IM.lookup v mm




-- -- Inc

-- data Inc a = Z a
--   | S a (Inc a)

-- lenInc :: Inc a -> Int
-- lenInc (Z _) = 0
-- lenInc (S _ s) = 1 + lenInc s

-- varInc :: Inc a -> a
-- varInc (Z x) = x
-- varInc (S x _) = x

-- lookInc i inc | lenInc inc == i = Just $ varInc inc




-- | 

-- data Expr v =
--     Var v
--   | App (Expr v) (Expr v)
--   | Lam (Expr (Incr v)) deriving (Eq, Show)



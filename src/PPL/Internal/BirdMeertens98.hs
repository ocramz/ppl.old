{-# language DeriveFunctor #-}
module PPL.Internal.BirdMeertens98 where

import qualified Data.Map as M
import qualified Data.IntMap as IM

import Control.Monad.Catch (MonadThrow(..), throwM)
import GHC.Exception
import Data.Typeable

import Data.Bifunctor

import Prelude hiding (lookup)

data Bind v =
    Zero
  | Succ v deriving (Eq, Show, Functor)

-- the paper uses 'Abs' ("abstraction") rather than 'Lam'
data Term v a =
  Var v | Const a | App (Term v a) (Term v a) | Lam (Term (Bind v) a)
  deriving (Eq, Show, Functor)

instance Bifunctor Term where
  bimap f g tt = case tt of
    Var v -> Var (f v)
    Const c -> Const (g c)
    App t1 t2 -> App (bimap f g t1) (bimap f g t2)
    Lam t -> Lam (bimap (f <$>) g t)

term :: (u -> v) -> Term u a -> Term v a
term = first    

abstract :: Eq v => Term v a -> v -> Term v a
abstract t x = Lam $ lift t x

lift :: Eq t => Term t a -> t -> Term (Bind t) a
lift (Var y) x
  | y == x = Var Zero
  | otherwise = Var (Succ y)
lift (App u v) x = App (lift u x) (lift v x)
lift (Lam t) x = Lam $ lift t (Succ x)
lift (Const c) _ = Const c

reduce :: Term v a -> Term v a -> Maybe (Term v a) 
reduce s0 t = case s0 of
  Lam s -> Just $ subst s t
  _ -> Nothing

subst :: Term (Bind v) a -> Term v a -> Term v a
subst (Const c) _ = Const c
subst (Var s) t = case s of
  Zero -> t
  Succ x -> Var x
subst (App u v) t = App (subst u t) (subst v t)
subst (Lam s) t = Lam $ subst s (term Succ t)




-- Bird, Meertens 98


-- data Term v = Var v | App (Term v) (Term v) | Abs (Term (Bind v)) deriving (Eq, Show, Functor)

-- abstract :: Eq v => Term v -> v -> Term v
-- abstract t x = Abs $ lift t x

-- lift :: Eq t => Term t -> t -> Term (Bind t)
-- lift (Var y) x
--   | y == x = Var Zero
--   | otherwise = Var (Succ y)
-- lift (App u v) x = App (lift u x) (lift v x)
-- lift (Abs t) x = Abs $ lift t (Succ x)

-- reduce :: Term v -> Term v -> Maybe (Term v)
-- reduce s0 t = case s0 of
--   Abs s -> Just $ subst s t
--   _ -> Nothing

-- subst :: Term (Bind v) -> Term v -> Term v
-- subst (Var s) t = case s of
--   Zero -> t
--   Succ x -> Var x
-- subst (App u v) t = App (subst u t) (subst v t)
-- subst (Abs s) t = Abs $ subst s (Succ <$> t)




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



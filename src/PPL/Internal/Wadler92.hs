module PPL.Internal.Wadler92 where

import qualified Data.Map as M
import qualified Data.IntMap as IM

import Control.Monad.Catch (MonadThrow(..), throwM)
import GHC.Exception
import Data.Typeable

import Prelude hiding (lookup)



data Expr v a =
    Var v
  | Const a
  | Plus (Expr v a) (Expr v a)
  | App (Expr v a) (Expr v a)
  | Lam (Expr v a)
  deriving (Eq, Show)

data Value m a =
    Val a
  | Fun (Value m a -> m (Value m a)) 

instance Show a => Show (Value m a) where
  show (Val x) = show x
  show _ = "error"

-- | Binary numeric operations
-- data Op2 a = Plus a a | Minus a a | Times a a deriving (Eq, Show)


-- ex0 = App (Lam ()) (Const 41)

ex0 :: Expr IM.Key Int
-- ex0 = App (Plus (Var 0) (Var 0)) $ Plus (Const 10) (Const 11)

ex0 = Plus (Const 2) (Const 3)

test :: Expr IM.Key Int -> Maybe (Value Maybe Int)
test = interp empty



add :: Num a => Value m1 a -> Value m2 a -> Maybe (Value m3 a)
add = liftOp2 (+)

liftOp2 :: (a -> b -> c)
        -> Value m1 a -> Value m2 b -> Maybe (Value m3 c)
liftOp2 f (Val a) (Val b) = pure $ Val (f a b)
liftOp2 _ _ _ = Nothing


interp :: Num a => Env (Value Maybe a) -> Expr IM.Key a -> Maybe (Value Maybe a)
interp env ex = case ex of
  Plus e1 e2 -> do
      a <- interp env e1
      b <- interp env e2
      add a b    
  Lam v -> pure $ Fun $ \a -> interp (augment a env) v
  App e1 e2 -> do
    f <- interp env e1
    e <- interp env e2
    apply f e 
  Const c -> pure $ Val c
  Var x -> lookup x env

apply :: MonadThrow m => Value m a -> Value m a -> m (Value m a)
apply v a = case v of
  Fun k -> k a
  _ -> throwM $ NotAFunction "Not a function"
  
data Except = NotAFunction String deriving (Eq, Show, Typeable)
instance Exception Except


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

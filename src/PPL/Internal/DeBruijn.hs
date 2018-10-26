module PPL.Internal.DeBruijn where

import qualified Data.Map as M
import qualified Data.IntMap as IM

import Control.Monad.Catch (MonadThrow(..), throwM)
import GHC.Exception
import Data.Typeable

import Prelude hiding (lookup)

data Incr v =
    Zero
  | Succ (Incr v) deriving (Eq, Show)

-- look _ Zero = Nothing
-- look e (Succ i) | e == i = pure e

data Expr v a =
    Var v
  | Const a
  | App (Expr v a) (Expr v a)
  | Lam (Expr (Incr v) a) deriving (Eq, Show)

data Value m a =
    Val a
  | Fun (Value m a -> m (Value m a))

interp :: Ord k =>
          M.Map k (Value Maybe a)
       -> Expr k a
       -> Maybe (Value Maybe a)
interp env ex = case ex of
  App e1 e2 -> do
    f <- interp env e1
    e <- interp env e2
    apply f e 
  Const c -> pure $ Val c
  Var x -> M.lookup x env

apply :: MonadThrow m => Value m a -> Value m a -> m (Value m a)
apply v a = case v of
  Fun k -> k a
  _ -> throwM $ NotAFunction "Not a function"
  
data Except = NotAFunction String deriving (Eq, Show, Typeable)
instance Exception Except



-- environment

newtype Env a = Env { unEnv :: IM.IntMap a } deriving (Eq, Show)

empty :: Env a
empty = Env IM.empty

size :: Env a -> Int
size = IM.size . unEnv

augment :: a -> Env a -> Env a
augment x (Env mm) = Env $ IM.insert k x mm where
  k = succ $ IM.size mm

lookup :: IM.Key -> Env a -> Maybe a
lookup v (Env mm) = IM.lookup v mm






-- | 

-- data Expr v =
--     Var v
--   | App (Expr v) (Expr v)
--   | Lam (Expr (Incr v)) deriving (Eq, Show)



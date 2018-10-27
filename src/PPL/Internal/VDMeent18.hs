{-# language LambdaCase #-}
module PPL.Internal.VDMeent18 where

import qualified Data.IntMap as IM
import qualified Data.Map as M
-- import qualified PPL.Internal.IEnv as IE

import Prelude hiding (lookup)

data Value m a =
    Val a
  | Fun (Value m a -> m (Value m a))   

data Expr v a =
    Const a
  -- | Var v
  | Let v (Expr v a)
  | App (Expr v a) (Expr v a)
  | Lam (Expr v a) 
  -- | If Bool (Expr v a) (Expr v a) -- 
  | Add (Expr v a) (Expr v a)
  -- | Mul (Expr v a) (Expr v a)   -- ...
  deriving (Eq, Show)

-- | 'defn f x = ...' == let f (lambda x .  ...)


-- interp env = \case
--   Const c -> pure $ Val c
--   -- Var x -> IE.lookup x env
--   Let v e



-- | Boolean expressions (i.e. that evaluate to a Boolean value)
data B v a =
    GeT (Expr v a) (Expr v a)
  | LeT (Expr v a) (Expr v a)
  | Eql (Expr v a) (Expr v a)
  deriving (Eq, Show)

interpB :: (Monad m, Ord a1) =>
           (t -> Expr v a2 -> m a1) -- ^ Eval.function for Expr
        -> t                        -- ^ Expr environment
        -> B v a2                   -- ^ Boolean expression
        -> m Bool   
interpB evale env = \case
  GeT e1 e2 -> do
    a <- evale env e1
    b <- evale env e2
    pure (a >= b)
    


-- |

newtype Env v a = Env (M.Map v a) deriving (Eq, Show)

instance Ord v => Semigroup (Env v a) where
  (<>) = union

instance Ord v => Monoid (Env v a) where
  mempty = empty

empty :: Env v a
empty = Env M.empty

union (Env e1) (Env e2) = Env $ M.union e1 e2

fromList :: (Foldable t, Ord v) => t (v, a) -> Env v a
fromList = foldl (flip $ uncurry augment) empty

augment :: Ord v => v -> a -> Env v a -> Env v a
augment k v (Env mm) = Env $ M.insert k v mm

lookup :: Ord k => k -> Env k a -> Maybe a
lookup k (Env mm) = M.lookup k mm

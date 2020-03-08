{-# language DeriveFunctor #-}
{-# language GADTs, KindSignatures #-}
module Lib where

-- algebraic-graphs
import Algebra.Graph (Graph, empty, vertex, edge, connect)
-- transformers
import Control.Monad.Trans.State (StateT, runStateT, execStateT, modify)

-- newtype G = G (StateT )

data NodeTy = Latent | Observed deriving (Eq, Show)

data Node = Node { nodeName :: String
                 , nodeTy :: NodeTy
                 , nodeDistrib :: Distribution } deriving (Eq, Show)

instance Ord Node where
  (Node n1 _ _) <= (Node n2 _ _) = n1 <= n2

-- newtype BNet = BNet { getBNet :: Graph Node } deriving (Eq, Show)

newtype BNetState m a = BNS { unBns :: StateT (Graph Node) m a }

-- sample from to = _ 

runBns b = execStateT (unBns b) empty


type D = Double

newtype Positive a = Positive { getPositive :: a } deriving (Eq, Show, Functor)

mkPositive :: (Ord a, Num a) => a -> Maybe (Positive a)
mkPositive x | x > 0 = Just (Positive x)
             | otherwise = Nothing

data Distribution = Uniform D D
                  | Exponential D
                  | Normal D (Positive D)
                  deriving (Eq, Show)






-- -- --


-- uniform :: Sample m => D -> D -> m D
-- uniform a b = sample (Uniform a b)
-- normal :: Sample m => D -> Positive D -> m D
-- normal m s = sample (Normal m s)
-- exponential :: Sample m => D -> m (Positive D)
-- exponential l = Positive <$> sample (Exponential l)


-- class Monad m => Sample m where
--   sample :: Distribution -> m D

-- class Monad m => Observe m where
--   observe :: D -> Distribution -> m ()


-- -- -- 







data Lam :: * -> * where
  Lift :: a                     -> Lam a        -- ^ lifted value
  Lam  :: (Lam a -> Lam b)      -> Lam (a -> b) -- ^ lambda abstraction
  App  :: Lam (a -> b) -> Lam a -> Lam b        -- ^ function application

eval :: Lam x -> x
eval (Lift v)   = v
eval (Lam f) = eval . f . Lift
eval (App f x)  = (eval f) (eval x)

-- -- data Expr a =
-- --     Const a
-- --   | Var String

-- type Var = String

-- data Expr a =
--     Const a
--   | Let (Var, Expr a) (Expr a)
--   deriving (Eq, Show)


-- lets :: Foldable t => t (Var, Expr a) -> Expr a -> Expr a
-- lets es e0 = foldr Let e0 es

-- -- eval e = case e of
-- --   Const x -> x




{-# language DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses, TypeFamilies #-}
module PPL.Internal.Literature.Hur15 where

import qualified Control.Monad.Trans.State as S
import qualified Data.Map.Strict as M
import Numeric.Log
import Prelude hiding (lookup)

-- | Univariate continuous distributions
data Dist1 a =
    Normal a a
  | Uniform a a
  deriving (Eq, Show)

newtype Theta k d1 d = Theta (M.Map k [(d1, Dist1 d)]) deriving (Eq, Show)

-- empty :: Theta k d
-- empty = Theta M.empty

-- lookup :: Ord k => k -> Theta k d -> Maybe [(d, Dist1 d)]
-- lookup k (Theta mm) = M.lookup k mm

-- append :: Ord k => k -> (d, Dist1 d) -> Theta k d -> Theta k d
append k v (Theta mm) = Theta $ M.insertWith (++) k [v] mm



newtype HistT k d1 d m a = HistT {
  unHistT :: S.StateT (Theta k d1 d) m a
  } deriving (Functor, Applicative, Monad)

class Observe m where
  observe :: String -> (Double, Dist1 Double) -> m ()
--   -- type D m :: *
--   observe :: String -> (D m, Dist1 (D m)) -> m ()

instance Monad m => Observe (HistT k d1 d m) where
--   -- type D (HistT d m) = d
  -- observe k v = HistT $ S.modify (append k v)  
  




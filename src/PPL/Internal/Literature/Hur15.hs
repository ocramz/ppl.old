{-# language DeriveFunctor, GeneralizedNewtypeDeriving #-}
module PPL.Internal.Literature.Hur15 where

import qualified Control.Monad.Trans.State as S
import qualified Data.Map.Strict as M

import Prelude hiding (lookup)

-- | Univariate continuous distributions
data Dist1 a =
    Normal a a
  | Uniform01
  deriving (Eq, Show)

newtype Theta k d = Theta (M.Map k [(d, Dist1 d)]) deriving (Eq, Show)

lookup k (Theta mm) = M.lookup k mm


newtype HistT k d m a = HistT {
  unHistT :: S.StateT (Theta k d) m a
  } deriving (Functor, Applicative, Monad)




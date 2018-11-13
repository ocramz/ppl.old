{-# language DeriveFunctor, GeneralizedNewtypeDeriving #-}
module PPL.Internal.MonadBayes where

import Numeric.Log

-- import Data.Number.LogFloat
import qualified Control.Monad.Trans.State as S
import qualified Control.Monad.Trans.Class as T
import qualified Control.Monad.Trans.Cont as C

import qualified System.Random.MWC.Probability as MWC

type R = Double

class Monad m => MonadSample m where
  uniform :: m R
  bernoulli :: R -> m Bool
  bernoulli p = fmap (< p) uniform
  normal :: R -> R -> m R

class Monad m => MonadCond m where
  score :: Log R -> m ()

class (MonadSample m, MonadCond m) => MonadInfer m


newtype W m a = W (S.StateT (Log R) m a) deriving (Functor, Applicative, Monad)

instance T.MonadTrans W where
  lift = W . T.lift

instance MonadSample m => MonadSample (W m) where
  uniform = T.lift uniform

mkW :: Monad m => (Log R -> (a, Log R)) -> W m a
mkW fs = W (S.state fs)

runW :: W m a -> m (a, Log R)
runW (W w) = S.runStateT w 1

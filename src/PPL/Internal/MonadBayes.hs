{-# language DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# language RankNTypes #-}
module PPL.Internal.MonadBayes where

import Numeric.Log

import Control.Monad.Primitive

-- import Data.Number.LogFloat
import qualified Control.Monad.Trans.State as S
import qualified Control.Monad.Trans.Class as T
import qualified Control.Monad.Trans.Cont as C

import qualified System.Random.MWC.Probability as MWC

randomWalk :: (MonadSample m, MonadCond m) => [R] -> m [R]
randomWalk ys = do
  s <- gamma 1 1
  let expand xss [] = return xss
      expand (x:xs) (y:ys) = do
        x' <- normal x s
        score (normalPdf x' 1 y)
        expand (x' : x : xs) ys
        where
          normalPdf m s z = Exp (1/(s*sqrt(s*pi))) - Exp ((z-m)**2/(2*s**2))
  xs <- expand [0] ys
  return $ reverse xs
          


-- 

type R = Double

class Monad m => MonadSample m where
  uniform :: m R
  bernoulli :: R -> m Bool
  bernoulli p = fmap (< p) uniform
  normal :: R -> R -> m R
  gamma :: R -> R -> m R
  -- normal mu sig = MWC.normal mu sig

instance PrimMonad m => MonadSample (MWC.Prob m) where
  uniform = MWC.uniform
  normal = MWC.normal
  gamma = MWC.gamma

class Monad m => MonadCond m where
  score :: Log R -> m ()

class (MonadSample m, MonadCond m) => MonadInfer m

-- | Weighting inference transformer
newtype W m a = W (S.StateT (Log R) m a) deriving (Functor, Applicative, Monad)

instance T.MonadTrans W where
  lift = W . T.lift

instance MonadSample m => MonadSample (W m) where
  uniform = T.lift uniform

mkW :: Monad m => (Log R -> (a, Log R)) -> W m a
mkW fs = W (S.state fs)

runW :: W m a -> m (a, Log R)
runW (W w) = S.runStateT w 1

instance Monad m => MonadCond (W m) where
  score w = W (S.modify (* w))

-- | Lift a natural transformation into a W
hoistW :: (forall x . m x -> n x) -> W m a -> W n a
hoistW f (W m) = W $ S.mapStateT f m


-- * tracing

-- | Rather than using the Church encoding of the free monad over the (r ->)
-- base functor as Scibior 2018, we use the equivalent 'Cont r'
data Tr r m a = Tr (W (C.Cont r) a) (m ([r], a))

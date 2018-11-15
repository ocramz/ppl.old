{-# language DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# language RankNTypes #-}
module PPL.Internal.MonadBayes where

import Numeric.Log

import Control.Monad.Primitive

-- import Data.Number.LogFloat
import qualified Control.Monad.Trans.State as S
import qualified Control.Monad.Trans.Class as T
import qualified Control.Monad.Trans.Cont as C
import qualified Control.Monad.Trans.Reader as R

import Control.Monad.ST

import qualified System.Random.MWC.Probability as MWC


-- linReg yss = do 

randomWalk :: (MonadSample m, MonadCond m) => [R] -> m [R]
randomWalk yss = do
  s <- gamma 1 1
  let expand xss [] = return xss
      expand xss@(x:_) (y:ys) = do
        x' <- normal x s
        score (normalPdf x' 1 y)
        expand (x' : xss) ys
  xs <- expand [0] yss
  return $ tail $ reverse xs
    
normalPdf :: (Precise a, RealFloat a) => a -> a -> a -> Log a
normalPdf m s z = Exp (1/(s*sqrt(2*pi))) + Exp (-(z-m)**2/(2*s**2))

runRandomWalk :: [R] -> ([R], Log R)
runRandomWalk xs = runSamplerW (randomWalk xs)

runSamplerW :: W SamplerST a -> (a, Log R)
runSamplerW = runSamplerST . runW


-- | One step of the MH algorithm

-- mhStep :: MonadSample m => (t -> Log R) -> (t -> m t) -> t -> m t
mhStep q p x = do
  x' <- q x
  let alpha = p x' / p x
      r = min 1 alpha
  accept <- bernoulli $ ln r
  let y | accept = x'
        | otherwise = x
  pure y

-- mhStep :: MonadSample m =>
--           R          -- ^ Acceptance rate (0 - 1)
--        -> (b -> m b) -- ^ Kernel
--        -> m b        -- ^ Sampler
--        -> m b
-- mhStep rho kern mx = do
--   x <- mx
--   x' <- kern x
--   p <- bernoulli rho
--   let y | p = x'
--         | otherwise = x
--   pure y
  
  



-- * Basic classes

type R = Double

class Monad m => MonadSample m where
  uniform :: m R
  bernoulli :: R -> m Bool
  bernoulli p = fmap (< p) uniform
  normal :: R -> R -> m R
  gamma :: R -> R -> m R

instance PrimMonad m => MonadSample (MWC.Prob m) where
  uniform = MWC.uniform
  normal = MWC.normal
  gamma = MWC.gamma

-- | Conditioning (i.e. accumulation of log-likelihood)
class Monad m => MonadCond m where
  score :: Log R -> m ()

class (MonadSample m, MonadCond m) => MonadInfer m


-- | Sampler 
newtype SamplerST a =
  SamplerST { unSamplerST :: forall s . R.ReaderT (MWC.GenST s) (ST s) a }
  deriving (Functor)

instance Applicative SamplerST where
  pure x = SamplerST $ pure x
  (SamplerST f) <*> (SamplerST x) = SamplerST $ f <*> x

instance Monad SamplerST where
  (SamplerST x) >>= f = SamplerST $ x >>= unSamplerST . f

instance MonadSample SamplerST where
  uniform = SamplerST $ do
    g <- R.ask
    T.lift $ MWC.sample uniform g
  gamma a b = SamplerST $ do
    g <- R.ask
    T.lift $ MWC.sample (gamma a b) g
  normal mu sig = SamplerST $ do
    g <- R.ask
    T.lift $ MWC.sample (normal mu sig) g
    
-- | Create a PRNG and run a Sampler with it
runSamplerST :: SamplerST a -> a
runSamplerST (SamplerST s) = runST $ do
  g <- MWC.create
  R.runReaderT s g

-- | Weighting inference transformer
newtype W m a = W (S.StateT (Log R) m a) deriving (Functor, Applicative, Monad)

instance T.MonadTrans W where
  lift = W . T.lift

instance MonadSample m => MonadSample (W m) where
  uniform = T.lift uniform
  gamma a b = T.lift (gamma a b)
  normal mu sig = T.lift (normal mu sig)

runW :: W m a -> m (a, Log R)
runW (W w) = S.runStateT w 1

-- execW :: Functor m => W m a -> m (Log R)
-- execW w = snd <$> runW w 

instance Monad m => MonadCond (W m) where
  score w = W (S.modify (* w))

-- | Lift a natural transformation into a W
hoistW :: (forall x . m x -> n x) -> W m a -> W n a
hoistW f (W m) = W $ S.mapStateT f m




-- * Traced

-- | Rather than using the Church encoding of the free monad over the (r ->)
-- base functor as Scibior 2018, we use the equivalent 'Cont r'

-- -- data Tr r m a = Tr {            -- 
-- --     trCont :: W (C.ContT r m) a   -- ^ I don't know how to use this object, yet
-- --   -- , trTrace :: m ([r], a)
-- --   } deriving (Functor)

-- data Tr r m a = Tr {
--     trCont :: W (C.ContT r m) a   -- ^ I don't know how to use this object, yet
--   -- , trTrace :: m ([r], a)
--   } -- deriving (Functor)

-- -- runTr :: Applicative m => Tr (a, Log R) m a -> m (a, Log R)
-- runTr (Tr trc) = C.runContT (runW trc) pure



-- * sampling in a continuation monad

-- newtype CG r a = CG { unCG :: C.ContT r SamplerST a } deriving (Functor, Applicative, Monad)

-- -- runCG (CG cg) = runSamplerST $ C.runContT cg pure


--

-- newtype A m a = A (W (C.ContT R m) a)

-- newtype B r a = B { unB :: W (C.ContT r SamplerST) a } deriving (Functor, Applicative, Monad)

-- runB (B bb) = runSamplerST $ C.runContT (runW bb) pure

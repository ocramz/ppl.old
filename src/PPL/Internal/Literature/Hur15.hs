{-|
Implementation of

Hur, Nori, Rajamani, Samuel - A provably correct sampler for probabilistic programs - 2015
-}
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

-- | A key-value store of lists of RV observations. Each key represents a distinct random variable, and the list elements are pairs of (observation, distribution with parameters)
newtype Obs k d1 d = Obs (M.Map k [(d1, Dist1 d)]) deriving (Eq, Show)

emptyObs :: Obs k d1 d
emptyObs = Obs M.empty

appendObs :: Ord k => k -> (d1, Dist1 d) -> Obs k d1 d -> Obs k d1 d
appendObs k v (Obs mm) = Obs $ M.insertWith (++) k [v] mm

newtype HistT m a = HistT {
  unHistT :: S.StateT (Obs String Double Double) m a
                          } deriving (Functor, Applicative, Monad)

class Monad m => Observe m where
  observe :: String -> (Double, Dist1 Double) -> m ()

class Monad m => Sample m where
  sample :: Dist1 a -> m a

bindS :: (Sample m, Observe m) => String -> Dist1 Double -> m ()
bindS k d = do
  v <- sample d
  k <~ (v, d)

-- | Append a sampled value and its distribution to a map, indexed by string names
--
-- e.g.
--
-- "x" <~ (0.3, Normal 0 1)
(<~) :: Observe m => String -> (Double, Dist1 Double) -> m ()
(<~) = observe  

instance Monad m => Observe (HistT m) where
  observe k v = HistT $ S.modify (appendObs k v)




-- newtype HistT k d1 d m a = HistT {
--   unHistT :: S.StateT (Theta k d1 d) m a
--   } deriving (Functor, Applicative, Monad)

-- class Observe m where
--   observe :: String -> (Double, Dist1 Double) -> m ()
-- --   -- type D m :: *
-- --   observe :: String -> (D m, Dist1 (D m)) -> m ()

-- instance Monad m => Observe (HistT k d1 d m) where
-- --   -- type D (HistT d m) = d
--   -- observe k v = HistT $ S.modify (append k v)  
  




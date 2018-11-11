{-# language DeriveGeneric #-}
module PPL.Internal.Bound where

import GHC.Generics

import Data.Functor.Classes (Show1(..), Show2(..), showsUnaryWith, showsPrec1)
-- import Control.Monad (liftM)

-- | From `bound`

-- | Var
data Var b a = B b | F a deriving (Eq, Show, Generic, Generic1)

instance Show2 Var where
  liftShowsPrec2 f _ _ _ d (B a) = showsUnaryWith f "B" d a
  liftShowsPrec2 _ _ h _ d (F a) = showsUnaryWith h "F" d a

instance Show b => Show1 (Var b) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

-- | Scope
newtype Scope b f a = Scope { unscope :: f (Var b (f a)) } deriving Generic

instance (Show b, Show1 f) => Show1 (Scope b f) where
  liftShowsPrec f g d m = showsUnaryWith (liftShowsPrec (liftShowsPrec f' g') (liftShowList f' g')) "Scope" d (unscope m) where
    f' = liftShowsPrec f g
    g' = liftShowList f g
  
instance (Show a, Show b, Show1 f) => Show (Scope b f a) where
  showsPrec = showsPrec1

-- >>> :m +Data.List
-- λ> abstract (`elemIndex` "bar") "barry"
-- 
-- Scope [B 0,B 1,B 2,B 2,F "y"]
abstract :: Applicative f => (a -> Maybe b) -> f a -> Scope b f a
abstract f e = Scope (k <$> e) where
  k y = case f y of
    Just z  -> B z
    Nothing -> F (pure y)



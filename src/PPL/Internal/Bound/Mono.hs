{-# language LambdaCase #-}
module PPL.Internal.Bound.Mono where

import Data.List (elemIndex)

-- | 

-- | Var
data Var b a =
  B b   -- ^ Bound variable
  | F a -- ^ Free variable
  deriving (Eq, Show)

-- | Scope
newtype Scope b a = Scope { unscope :: [ Var b [a] ] } deriving (Eq, Show)


-- | Capture some free variables in an expression to yield
-- a 'Scope' with bound variables in @b@
-- 
-- >>> abstract (`elemIndex` "bar") "barry"
-- Scope {unscope = [B 0,B 1,B 2,B 2,F "y"]}
abstract :: (a -> Maybe b) -> [a] -> Scope b a
abstract f e = Scope (k <$> e) where
  k y = case f y of
    Just z  -> B z
    Nothing -> F (pure y)
{-# INLINE abstract #-}

instantiate :: (b -> [a]) -> Scope b a -> [a]
instantiate k e = unscope e >>= \case
  B b -> k b
  F a -> a
{-# INLINE instantiate #-}


-- | Enter a 'Scope' that binds one variable, instantiating it
--
-- >>> instantiate1 "x" $ Scope [B (),F "y",F "z"]
-- "xyz"
instantiate1 :: [a] -> Scope b a -> [a]
instantiate1 e = instantiate (const e)

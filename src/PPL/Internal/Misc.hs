{-# language MultiParamTypeClasses, FunctionalDependencies #-}
module PPL.Internal.Misc where


-- | Random linear functions, monadic version
linFun :: (Monad m, Num a) => m a -> m a -> m (a -> a)
linFun ma mb = do
  a <- ma
  b <- mb
  return $ \x -> a * x + b

-- | Random linear functions, applicative version
linFun' :: (Applicative f, MV mat vec) => f mat -> f vec -> f (vec -> vec)
linFun' ma mb = (\a b x -> (a #> x) ^+^ b) <$> ma <*> mb

newtype LinFun v = LF (v -> v)

class V v where
  (^+^) :: v -> v -> v

class V v => MV m v | v -> m where
  (#>) :: m -> v -> v

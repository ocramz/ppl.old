module PPL.Internal.IEnv where

import qualified Data.IntMap as IM

-- environment, encoded as IntMap

newtype IEnv a = IEnv { unEnv :: IM.IntMap a } deriving (Eq, Show)

empty :: IEnv a
empty = IEnv IM.empty

size :: IEnv a -> Int
size = IM.size . unEnv

mkEnv :: Foldable t => t a -> IEnv a
mkEnv xs = foldl (flip augment) empty xs

augment :: a -> IEnv a -> IEnv a
augment x (IEnv mm) = IEnv $ IM.insert k x mm where
  k = IM.size mm

lookup :: IM.Key -> IEnv a -> Maybe a
lookup v (IEnv mm) = IM.lookup v mm

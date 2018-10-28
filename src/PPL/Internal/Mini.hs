{-# language LambdaCase #-}
module PPL.Internal.Mini where

-- | Value type
data Value a =
    Val a
  | Fun (Value a -> Value a)

-- | Primitive operations
data OP = Plus | Times deriving (Eq, Show)

-- | Expression type
data Expr v a =
    v :-> Expr v a       -- ^ Lambda
  | Expr v a :$ Expr v a -- ^ Apply
  -- | Prim OP (Expr v a) (Expr v a) -- ^ Primitive operation (e.g. '+')
  | Const a
  deriving (Eq, Show)

-- | 'Let' is equivalent to applying a lambda
let_ :: v -> Expr v a -> Expr v a -> Expr v a
let_ v e body = (v :-> body ) :$ e

lets_ :: Foldable t => t (v, Expr v a) -> Expr v a -> Expr v a
lets_ decls body = foldl (flip $ uncurry let_) body decls

-- | Evaluating an 'Expr' produces a 'Value'
eval = \case
  Const x -> Val x
  v :-> body -> undefined

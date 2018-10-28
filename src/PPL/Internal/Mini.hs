module PPL.Internal.Mini where



data Expr v a =
    v :-> Expr v a       -- Lambda
  | Expr v a :$ Expr v a -- Apply
  | Const a
  deriving (Eq, Show)

-- | 'Let' is equivalent to applying a lambda
let_ :: v -> Expr v a -> Expr v a -> Expr v a
let_ v e body = (v :-> body ) :$ e

lets_ :: Foldable t => t (v, Expr v a) -> Expr v a -> Expr v a
lets_ decls body = foldl (flip $ uncurry let_) body decls

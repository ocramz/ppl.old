{-# language GADTs #-}
{-# language NoMonomorphismRestriction #-}
{-# language LambdaCase #-}
{-# language DeriveFunctor #-}
module PPL.Internal.HOAS1 where



-- data Exp2 a b =
--     V a
--   | Lam (a -> Exp2 a b)
--   | Abs (Exp2 a b) (Exp2 a b)



-- 

-- data E a =
--     Term a
--   | E a :$ E a
--   | Abs (E a -> E a)



-- ("PHOAS for Free" blogpost - https://www.schoolofhaskell.com/user/edwardk/phoas )

data Rec p a b =
    Place b                -- ^ "Placeholder" for a value
  | Roll (p a (Rec p a b))

instance Functor (Rec p a) where
  fmap f (Place x) = Place (f x)
  -- fmap f (Roll ps) = Roll _ -- $ lmap f ps
instance Applicative (Rec p a) where
  pure = Place
  -- (<*>) = _

instance Monad (Rec p a) where
  return = pure

data ExpF a b =
    VarF a
  | AbsF (a -> b)
  | AppF b b
  | PlusF b b

type Exp a b = Rec ExpF a b

let_ :: Exp a b -> (a -> Exp a b) -> Exp a b
let_ x body = lam body `app` x

lam :: (a -> Exp a b) -> Exp a b
lam f = Roll (AbsF f)

app :: Exp a b -> Exp a b -> Exp a b 
e1 `app` e2 = Roll (AppF e1 e2)

var :: a -> Exp a b
var x = Roll (VarF x)

plus :: Exp a b -> Exp a b -> Exp a b 
plus e1 e2 = Roll (PlusF e1 e2)

cata :: Profunctor p => (p b d -> d) -> Rec p b d -> d
cata _   (Place b) = b
cata phi (Roll bs) = phi (rmap (cata phi) bs)


-- | This cannot have a pure value constructor, otherwise pattern matching in evalAux becomes partial and everything breaks
newtype Value a = Fn { unFn :: Value a -> Value a }

eval :: Exp (Value a) (Value a) -> Value a
eval = cata evalAux

evalAux :: ExpF (Value a) (Value a) -> Value a
evalAux e = case e of
  AbsF f   -> Fn f
  AppF f x -> unFn f x
  VarF x   -> Fn $ const x   -- ?!
  -- PlusF a b -> Fn $ const (a + b)  -- wrong




instance Profunctor ExpF where
  -- dimap f g (VarF x)   = VarF (f x)  -- ?
  dimap _ g (AppF x y) = AppF (g x) (g y)
  dimap f g (AbsF h)   = AbsF (g . h . f)


bindExpF (Roll bs) f = Roll $ rmap (>>= f) bs

bindP :: (Profunctor p, Monad m) =>
         p x (m a) -> (a -> m b) -> p x (m b)
bindP bs f = rmap (>>= f) bs


class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

lmap :: Profunctor p => (a -> b) -> p b d -> p a d
lmap f = dimap f id

rmap :: Profunctor p => (c -> d) -> p b c -> p b d
rmap = dimap id

  

-- --

-- data Exp a where
--   (:->) :: Exp a -> (Exp a -> Exp b) -> Exp (a -> b)
--   (:$) :: Exp (a -> b) -> Exp a -> Exp b    -- (<*>)
--   Plus :: Exp a -> Exp a -> Exp a
--   Const :: a -> Exp a

  
-- let_ :: Exp a -> (Exp a -> Exp b) -> Exp a -> Exp b
-- let_ x fx body = (x :-> fx) :$ body

-- plus :: Num a => Exp a -> Exp a -> Exp a
-- plus = Plus

-- konst :: a -> Exp a
-- konst = Const


-- interp = \case
--   Const x  -> x
--   Plus a b -> interp a + interp b


-- t0 = let_ (konst 21) (plus $ konst 21)

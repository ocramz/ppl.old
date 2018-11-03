module PPL.Internal.WashburnWeirich where

{- | Encoding higher order abstract syntax with parametric polymorphism - Wasburn, Weirich 2003
-}

-- Base functor (invariant since type param 'a' is present both in co- and contra-variant positions)
--
-- Untyped lambda calculus, with a function object inside (aka an "exponential" type)
data ExpF a =
    AbsF (a -> a)
  | AppF a a
  | ConstF a
  | PlusF a a

-- Fegaras-Sheard construction
data Rec a =
    Roll (ExpF (Rec a))
  | Place a

-- lambda abstraction
lam :: (Rec a -> Rec a) -> Rec a
lam f = Roll (AbsF f)

app :: Rec a -> Rec a -> Rec a
app a b = Roll (AppF a b)

-- "invariant" map
xmap :: (t -> a, a -> t) -> (ExpF t -> ExpF a, ExpF a -> ExpF t)
xmap (f, g) = (lf, lg) where
  lf x = case x of
    AbsF fx    -> AbsF (f . fx . g)
    AppF e1 e2 -> AppF (f e1) (f e2)
    ConstF xx   -> ConstF (f xx)
    PlusF e1 e2 -> PlusF (f e1) (f e2)
  lg x = case x of
    AbsF fx    -> AbsF (g . fx . f)
    AppF e1 e2 -> AppF (g e1) (g e2)
    ConstF xx   -> ConstF (g xx)
    PlusF e1 e2 -> PlusF (g e1) (g e2)    
  
-- cata :: (ExpF p -> p) -> Rec p -> p
cata f (Roll fx) = f $ fst (xmap (cata f, Place)) $ fx
cata _ (Place x) = x

--

-- the interpretation function needs a single step evaluation, not a recursive call

-- eval = cata evalAux

newtype Value a = Value { unValue :: Either a (Value a -> Value a) }

-- data Value a = Val a | Fn (Value a -> Value a)

-- evalAux :: ExpF (Value a) -> Maybe (Value a)
-- evalAux (AbsF f) = Just $ Fn f
-- evalAux (AppF fm x) = unFn fm >>= \f -> pure (f x)

-- unFn :: Value a -> Maybe (Value a -> Value a)
-- unFn (Fn f) = Just f
-- unFn (Val _) = Nothing





sumAux e = case e of
  ConstF x -> x
  PlusF a b -> a + b
  

-- countAux (AppF a b) = a + b
-- countAux (AbsF f) = f 1
-- countAux (ConstF x) = x
-- -- countAux (PlusF a b) = 

-- count :: Rec Integer -> Integer
-- count = cata countAux




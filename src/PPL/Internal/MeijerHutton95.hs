{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
{-# language DeriveFunctor #-}
module PPL.Internal.MeijerHutton95 where

class Retract b a where
  up :: b -> a
  down :: a -> b

class Retract a (a -> a) => Reflexive a where
  apply :: a -> (a -> a)
  abstr :: (a -> a) -> a


data E =
    Var String
  | Lambda String E
  | Apply E E

eval :: Reflexive a => E -> Env a -> a
eval (Var x) env = look env x
eval (Lambda x b) env = abstr $ \a -> eval b (env `update` (x, a))
eval (Apply e1 e2) env = eval e1 env `apply` eval e2 env

  



-- | Environment interface as functions
newtype Env a = Env { unEnv :: String -> a } deriving (Functor)

-- | Lookup 
look :: Env a -> String -> a
look env x = unEnv env x

-- | Update
update :: Env p -> (String, p) -> Env p
update env (x, a) = Env $ \y -> if y == x then a else look env y


data C a b = Clos { runClos ::
  ((a -> b) -> a) -> ((b -> a) -> b)
                  }

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

-- instance Profunctor C where
--   dimap f g (Clos h) = Clos (((f `dimap` g) `dimap` f) `dimap` ((g `dimap` f) `dimap` g)) h

-- spork f g (Clos h) =  (((f `dimap` g) `dimap` f) `dimap` ((g `dimap` f) `dimap` g)) h 

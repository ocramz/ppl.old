module PPL.Internal.TaglessFinal1 where



{-| refs:

Free monads considered harmful : https://markkarpov.com/post/free-monad-considered-harmful.html

Introduction to PPL : https://arxiv.org/abs/1809.10756

-}

-- | Intro PPL notes that evaluation of 'sample' and 'observe' terms depends on the actual inference algorithm used. Therefore we try declaring them here as an abstract interface.
class Observe m where
  sample :: m b
  observe :: a -> m b
  

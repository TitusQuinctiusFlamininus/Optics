module DownstarFun where

import Control.Lens.Combinators (Profunctor, dimap)

{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b
  rmap  :: (b -> d) -> p a b -> p a d
  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d

class Functor w => Comonad w where
    extract :: w a -> a


--}

-- Imagining a type that encompasses the idea of a Downstar
newtype OpticalDownstar f a b = OpDownstar { downer :: f a -> b }

-- Ok, lets go ahead and make it a Profunctor
instance Functor f => Profunctor (OpticalDownstar f) where
  dimap h g ( OpDownstar d ) = OpDownstar (  g . d . fmap h )
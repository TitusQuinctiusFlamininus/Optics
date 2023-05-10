module UpstarMonoidalFun where

import Control.Lens.Combinators (Profunctor, dimap)


{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b
  rmap  :: (b -> d) -> p a b -> p a d
  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d

class Functor f where
    fmap :: (a -> b) -> f a -> f b

--}

-- Ok, Let us invent our own Upstar
newtype MonoStar f  a  b      =   MonoidalStar { unstar ::  a  ->  f b }


-- Making it a Profunctor 
instance Functor f =>  Profunctor (MonoStar f) where
    dimap h g (MonoidalStar r)          =    MonoidalStar (fmap g . r . h) 

module DownstarFun where

import Control.Lens.Combinators (Profunctor, dimap)

{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b
  rmap  :: (b -> d) -> p a b -> p a d
  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d

--This may come in useful, given the nature of a Downstar
class Functor w => Comonad w where
    extract :: w a -> a


--}

-- Imagining a type that encompasses the idea of a Downstar
newtype OpticalDownstar f a b = OpDownstar { downer :: f a -> b }

-- Ok, lets go ahead and make it a Profunctor
-- Whatever d produces, it needs to be a functor of something, so we have to fmap h
instance Functor f => Profunctor (OpticalDownstar f) where
  dimap h g ( OpDownstar d ) = OpDownstar (  g . d . fmap h )

  ---------------------------------------------------------------------------------

-- Ok, lets us come up with some examples of types to use for our downstar transformation

-- this will help be our Downstar functor
newtype OpFunc a            = OpFunc a 

-- we will be going from this type...
newtype From a              = From a

-- and eventually end up with this type.... 
newtype To a                = To a 

---------------------------------------------------------------------------------

-- Since the input of our downer is a functor of something, then let's make the type we've chosen to represent that functor, into one
instance Functor OpFunc where
    fmap f (OpFunc x)        = OpFunc (f x)

---------------------------------------------------------------------------------

-- Let us come up with some functions we can use for our dimap instance

-- Lets invent a contravariant function that provides the dimap something of type (f a), from the functional expression: (f a -> b)
preDownstar :: k   -> OpFunc (From a) 
preDownstar  = undefined

-- We also need a covariant function that takes our profunctor output (To a) and 
-- potentially manipulates it further (here (To a) represented abstractly as b, from the functional expression: (f a -> b)
postDownstar :: To a -> s'
postDownstar = undefined

-- Now we invent a function that represents how our Downstar works primarily
downer' :: OpFunc (From a)  -> To b
downer'  = undefined
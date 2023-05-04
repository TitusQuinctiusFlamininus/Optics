module DownstarFun where

import Control.Lens.Combinators  (Profunctor, dimap)
import Control.Comonad           (Comonad, extract, extend, duplicate)

{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b
  rmap  :: (b -> d) -> p a b -> p a d
  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d


--This may come in useful, given the nature of a Downstar
class Functor w => Comonad w where
    extract     ::  w a -> a                       <--- we need only this, technically
    duplicate   ::  w a -> w (w a)
    extend      :: (w a -> b) -> w a -> w b


--}

-- Imagining a type that encompasses the idea of a Downstar
newtype OpticalDownstar f a b          = OpDownstar { downer :: f a    ->    b }



-- Ok, lets go ahead and make it a Profunctor
-- Whatever d consumes, it needs to be a functor of something, so we have to fmap h
instance Functor f => Profunctor (OpticalDownstar f) where
  dimap h g ( OpDownstar d )           = OpDownstar (  g . d . fmap h )



  ---------------------------------------------------------------------------------

-- Ok, lets us come up with some examples of types to use for our downstar transformation



-- this will help be our Downstar Input Computational type 
newtype OpFunc a            = OpFunc a 



-- we will be going from this type...
newtype From a              = From a



-- and eventually end up with this type.... 
newtype To a                = To a 



---------------------------------------------------------------------------------

-- Since the input of our downer is a functor of something, then let's make the type we've chosen to represent that functor, into one
instance Functor OpFunc where
    fmap f (OpFunc x)           = OpFunc (f x)




-- Just to give an example of the implementation of the Upstar internals, we'll go ahead and make our OpFunc type a comonad as well
-- For the comonad instance, if you implement extract, that's enough; but lets do them all (we're having fun after all :))
instance Comonad OpFunc where
    extract    (OpFunc x)       =  x
    duplicate  x                =  OpFunc x
    extend     f                =  fmap f . duplicate



---------------------------------------------------------------------------------

-- Let us come up with a few functions we can use for our dimap instance and build up one example of how to use the Downstar

-- Lets invent a contravariant function that provides the dimap something of type (f a), from the functional expression: (f a -> b)
preDownstar ::   m                 ->   From a
preDownstar  = undefined




-- We also need a covariant function that takes our profunctor output (To a) and 
-- potentially manipulates it further (here (To a) represented abstractly as b, from the functional expression: (f a -> b)
postDownstar ::  To a              ->    s'
postDownstar = undefined




-- Now we invent a function that represents how our Downstar works primarily
-- As mentioned before, since OpFunc is a comonad, and example of an implementation here is to convert what's extracted into our Downstar output type
downer'      ::  OpFunc (From a)    ->  To a
downer' p          = To x
  where ( From x ) = extract p



---------------------------------------------------------------------------------

--Time to tranform the Downstar instance into a Profunctor using functions we invented
proDownstar :: OpticalDownstar OpFunc m (To a)
proDownstar = dimap preDownstar postDownstar (OpDownstar downer')



-- Now its time to use the Downstar. All we need to do is provide something of type (OpFunc m)
-- since we are 'fmapping' h, we need to give the profunctor a functor computational type
-- we see that: fmap (m -> From a) -> OpFunc m -> OpFunc (From a)        <<----and this looks like what our Downstar (downer') takes
useProDownstar :: OpticalDownstar OpFunc m (To a)        ->        OpFunc m -> To a
useProDownstar (OpDownstar d)    = d



---------------------------------------------------------------------------------

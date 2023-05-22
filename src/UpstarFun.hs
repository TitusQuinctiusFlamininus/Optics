module UpstarFun where

import Control.Lens.Combinators (Profunctor, dimap)

{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b                 <<-----
                                                             |-------- Either implement these two ...
  rmap  :: (b -> d) -> p a b -> p a d                 <<-----  

  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d     <<-------------- Or just this one


class Functor f where
    fmap :: (a -> b) -> f a -> f b

--}


-- Ok, Let us invent our own Upstar
newtype OpticalUpstar f' a' b'      =   OpticalUpstar { unstar ::   a'   ->    f' b' }



-- Make it a Profunctor
-- Whatever exits u is a functor of something, and we already have a function g, which means we have to fmap
instance Functor f' => Profunctor (OpticalUpstar f') where
    dimap h g (OpticalUpstar u)     =   OpticalUpstar (fmap g . u . h) 





---------------------------------------------------------------------------------

-- Lets invent more types to use in our example

-- this will help be our functor
newtype OpFunc a            = OpFunc a 



-- we will be going from this type...
newtype From a              = From a



-- and eventually end up with this type.... 
newtype To a                = To a 


---------------------------------------------------------------------------------

--First let's make our OpFunc a functor, since it will be represented in f'
instance Functor OpFunc where
    fmap f (OpFunc x)        = OpFunc (f x)



---------------------------------------------------------------------------------

-- So now lets invent some functions that can take advantage of our types

-- Lets invent a contravariant function that provides the dimap something of type a
preUpstar :: k          ->       From a
preUpstar  = undefined



-- We also need a covariant function that takes our profunctor output (b) and potentially manipulates it further 
postUpstar :: To a      ->       s'
postUpstar = undefined



-- Now we invent a function that represents how our upstar works primarily
unstarter :: From a     ->       OpFunc (To b)
unstarter  = undefined


---------------------------------------------------------------------------------

-- Time to define our transformation
-- The old transformation, before it was a profunctor, used to go from : a' -> f b'
-- The new transformation, after it became a profunctor, goes from     : s -> s'
-- Actually, s' has the type (OpFunc s') so that we can infer: 
-- fmap :: (To a -> s') -> OpFunc (To b)  -> (OpFunc s')
proUpstarP :: OpticalUpstar OpFunc k (To b)
proUpstarP = dimap preUpstar postUpstar (OpticalUpstar unstarter)



-- So now you can use our Profunctor transformer (or any similar one), like this: 
-- Since we are going from : s -> s' , and not from some a', then lets just supply the s (which we are now calling type k)
useUpstart :: OpticalUpstar OpFunc k (To b)     ->    k     -> OpFunc (To b)
useUpstart (OpticalUpstar t) = t



---------------------------------------------------------------------------------

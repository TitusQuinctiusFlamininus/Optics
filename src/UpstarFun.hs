module UpstarFun where

import Control.Lens.Combinators (Profunctor, dimap)

{--

class Profunctor p where
    dimap :: (a' -> a) -> (b -> b')-> p a b -> p a' b'

class Functor f where
    fmap :: (a -> b) -> f a -> f b

--}

-- Let us now invent our own Upstar
data OpticalUpstar f' a' b' = OpticalUpstar {unstar :: a' -> f' b'}


--Make it a Profunctor
-- Whatever is outputted from u is a functor of something, and we already have our fmap function g, we have to fmap!
instance Functor f' => Profunctor (OpticalUpstar f') where
    dimap h g (OpticalUpstar u)     =  (OpticalUpstar (fmap g . u . h) ) 

---------------------------------------------------------------------------------

-- Lets invent more types to use in our example

-- this will help be our functor
newtype OpFunc a            = OpFunc a 

-- we will be going from this type to a (To) type
newtype From a              = From a

-- we are going to this type 
newtype To a                = To a 

---------------------------------------------------------------------------------

--First let's make our OpFunc a functor, since it will be represented in f'

instance Functor OpFunc where
    fmap f (OpFunc x)        = OpFunc (f x)

---------------------------------------------------------------------------------

-- So now lets invent some functions that can take advantage of our types

-- Lets invent a contravariant function that provides our dimap input type a
preUpstar :: s   -> From a
preUpstar  = undefined

-- We also need a covariant function that takes our profunctor output (b) and potentially manipulates it further 
postUpstar :: To a -> s'
postUpstar = undefined

-- Now we invent a function that represents how our upstar profunctor works primarily
--upStarter :: 
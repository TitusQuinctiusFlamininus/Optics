module UpstarFun where

import Control.Lens.Combinators (Profunctor, dimap)

{--

class Profunctor p where
    dimap :: (a' -> a) -> (b -> b')-> p a b -> p a' b'


--}

-- Our datatype 
data Upstar f a b = Upstar { unUpstar :: a -> f b }


--Make it a Profunctor
-- Whatever is outputted from u is a functor of something, and we already have our fmap function g, we have to fmap!
instance Functor f => Profunctor (Upstar f) where
    dimap h g (Upstar u)     =  (Upstar (fmap g . u . h) ) 


-- Let us now invent our own Upstar
data OpticalUpstar f' a' b' = OpticalUpstar {unstar :: a' -> f' b'}
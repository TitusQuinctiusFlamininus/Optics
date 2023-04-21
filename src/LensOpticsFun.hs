module LensOpticsFun where


import Control.Lens.Combinators (Profunctor, dimap)

{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b
  rmap  :: (b -> d) -> p a b -> p a d
  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d

where p is a Profunctor : 
type Optic p a b s t = p a b -> p s t

--}

-- This time we are dealing with the powerful concept of a lens
-- Let's roll our own 
data OpticalLens a b s t = OptLens { look :: s      -> a, 
                                     edit :: (b, s) -> t 
                                   }

-- Turning our custom type into a Profunctor
instance Profunctor (OpticalLens a b) where
    dimap h g (OptLens l e )     = OptLens (l . h) (\(x,y)  -> g $ e (x, h y))
        



prom   :: c  ->  a  -> (a, c)
prom y x = (x,y)

dem    :: c  ->  a -> (c, a)
dem  y x = (y,x)
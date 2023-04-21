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
data OpticalLens a b s t = OptLens { look :: s         ->  a, 
                                     edit :: (b, s)    ->  t 
                                   }

-- Turning our custom type into a Profunctor
instance Profunctor (OpticalLens a b) where
    dimap h g (OptLens l e )     = OptLens (l . h) (\(x,y)  -> g . e $ (x, h y))
        


---------------------------------------------------------------------------------

-- As is now also routine, we'll invent types and functions to use for a practical example

-- Let's define some type from which we can compose things of type s
data Atom                   =   Atom

-- Another definition that can represent original whole structured types
newtype Composite a         = Composite a

-- Some type we can use to build up new structured types
data Molecule               = Molecule

--Finally some new structure we can build up
newtype NewComposite b      = NewComposite b





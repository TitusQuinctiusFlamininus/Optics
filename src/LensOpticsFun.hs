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

-- Explanation: 
    -- (For OptLen's left-hand-side function ): We know that l takes composite types s, and h produces them: so we simply pair those types as (l . h)
    -- (For OptLen's right-hand-side function): 
        --  We need a function that takes a tuple and produces some type t, so we can invent this: (\(r,q) -> something)
        --  q looks a lot like s in the tuple, so it could be an s, .....or.......it could be something from which we can make things of type s (like something of type c, from the dimap definition)
        --  We need an s: If we give h something of type c, then we can produce an s. So let's assume q is identical to a c: It means: (h $ y) = s
        --  x could represent things of type b; So. (x, h y) could be :  (b, s)
        --  What consumes (b, s) ? Something of type e : So : e (x, h y) looks like t
        --  g is covariant. so let's give it the results of e (x, h y) so it may produce something of type d (from the dimap definition)
        


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



---------------------------------------------------------------------------------

-- Now to invent some functions 

-- This contravariant function will supply our original structure
preTreat     :: m   ->  Composite Atom
preTreat   = undefined


-- This covariant function will absorb our resultant type structures and possibly modify them further
postTreat    :: NewComposite Molecule  ->  n
postTreat  = undefined

-- Let's define a function that acts as a magnifying glass
peep         ::   Composite Atom  ->   Atom
peep      = undefined


-- Let's define a function that can assemble new types and create new ones from parts
comp         ::   (Molecule, Composite Atom)  ->   NewComposite Molecule
comp      = undefined

---------------------------------------------------------------------------------

-- Formulating a concrete profunctor type based our custom types
telescope :: OpticalLens Atom Molecule (Composite Atom) (NewComposite Molecule)
telescope = OptLens (peep . preTreat) (\(a',c')  -> postTreat . comp $ (a', preTreat c'))
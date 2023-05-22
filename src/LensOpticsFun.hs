module LensOpticsFun where


import Control.Lens.Combinators (Profunctor, dimap   )
import Control.Comonad          (Comonad   , duplicate, extract, extend )

{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up


class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b                 <<-----
                                                             |-------- Either implement these two ...
  rmap  :: (b -> d) -> p a b -> p a d                 <<-----  

  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d     <<-------------- Or just this one


where p is a Profunctor : 
type Optic p a b s t = p a b -> p s t


class Functor f where
    <$> :: (a -> b) -> f a -> f b


class Functor w => Comonad w where
    extract     ::  w a -> a      
    duplicate   ::  w a -> w (w a)
    extend      :: (w a -> b) -> w a -> w b                 

--}



---------------------------------------------------------------------------------


-- This time we are dealing with the powerful concept of a lens
-- Let's roll our own 
data OpticalLens a b s t         = OptLens { look :: s         ->     a, 

                                             edit :: (b, s)    ->     t 
                                           }



-- Turning our custom type into a Profunctor
instance Profunctor (OpticalLens s t) where
    dimap h g (OptLens l e )     = OptLens (l . h) (\x  -> g . e $ (fst x, h . snd $ x))

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
data Atom                     =   Atom



-- Another definition that can represent original whole structured types
newtype Composite a           = Composite a



-- Some type we can use to build up new structured types
data Molecule                 = Molecule



--Finally some new structure we can build up
newtype NewComposite b        = NewComposite b


---------------------------------------------------------------------------------
-- Defining the instances 

--Let's make the Composite a Functor, in the hope of making it a Comonad, will explain lower down
instance Functor Composite where
    fmap f (Composite b)      =  Composite (f b)



instance Comonad Composite where
    extract (Composite b)     =  b
    duplicate  x              =  Composite x
    extend     f              =  fmap f . duplicate


---------------------------------------------------------------------------------


-- Now to invent some functions 

-- This contravariant function will supply our original structure
preTreat     ::   m                              ->  Composite Atom
preTreat                = undefined


-- This covariant function will absorb our resultant type structures and possibly modify them further
postTreat    ::   NewComposite Molecule          ->   n
postTreat               = undefined


-- Let's define a function that acts as a magnifying glass, but we can use our comonad function
peep         ::   Composite Atom                ->   Atom
peep                    = extract


-- Let's define a function that can assemble new types and create new ones from parts
comp         ::   (Molecule, Composite Atom)    ->   NewComposite Molecule
comp                    = undefined


---------------------------------------------------------------------------------


-- Formulating a concrete profunctor type based our custom types
telescope :: OpticalLens Atom Molecule s t
telescope                    = OptLens (peep . preTreat) (\z  -> postTreat . comp $ (fst z, preTreat $ snd z))


-- Let's create the Optic: 
-- Now we see the use of extract in the Optic's general definition: We needed to step-down from a Functor/Comonad of some type to that type
-- Also notice that it does not matter what we used to do on the left hand side of our profunctor (almost like the Adapter case), but because the input type of the right-hand side it not trivial, the identity function is not enough 
-- Left-Hand side: We had (s -> a) : and we had (p a b)      ------->>> Means if we have: (p s t) we have to replace a's with s , but if we do that in (s -> a), we will end up with (s -> s), and that is the identity function!
-- Right-Hand side: v seems to simulate the type to modify our structure with; w seems to be a composite of the old type, but in order to give this tuple to macro, we need to extract w first    
teleOptic  :: OpticalLens a Molecule Atom Molecule          ->        OpticalLens Atom Molecule (Composite a) (NewComposite Molecule)
teleOptic (OptLens _ macro)   = dimap preTreat postTreat $ OptLens peep (\x  -> NewComposite $ macro (fst x, peep . snd $ x))



---------------------------------------------------------------------------------


-- How can we use the new Profunctor to peer into some composite type
microscope :: Composite Atom        ->        Atom
microscope                = look (teleOptic telescope)


-- How can we use the new Profunctor to build up some new composite type from smaller alternative parts
structer   :: Molecule              ->       Composite Atom       ->      NewComposite Molecule
structer m o              = edit (teleOptic telescope) (m, o)


---------------------------------------------------------------------------------
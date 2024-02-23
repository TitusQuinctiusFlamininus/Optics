module LensMonoidalOpticsFun where 



import Control.Lens.Combinators    (Profunctor, dimap )

{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up


class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b                 <<-----
                                                             |-------- Either implement these two ...
  rmap  :: (b -> d) -> p a b -> p a d                 <<-----  

  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d     <<-------------- Or just this one


class Profunctor p => Monoidal p where
  par         :: p a b    ->   p c d    ->  p (a, c) (b, d)
  empty       :: p () ()


where p is a Profunctor : 
type  Optic  p  a  b  s  t    =  p  a  b    ->   p  s  t


--}



---------------------------------------------------------------------------------

-- Base definition for the type
class Profunctor  p => Monoidal p where
  par        ::   p a b     ->    p c d     -> p (a, c) (b, d)
  empty      ::   p () ()


-- Raising the lens definition
data  MonoLens  a  b  s  t     =     MLens    {   blik   ::  s       ->   a,

                                                   upp   ::  (b, s)  ->   t
                                              }


-- First establishing the Profunctor...
instance Profunctor (MonoLens a b) where
        dimap  h  g  (MLens r  z)        =    MLens   ( r . h ) (\x  ->  g $ z (fst x, (h $ snd x)))


-- Now, attempting to make the Lens Monoidal...
instance Monoidal (MonoLens a b) where
    par (MLens k  m) (MLens _  r)        =    MLens (k . fst) (\y -> ((m (fst y, fst . snd $ y)) , r (fst y, snd . snd $ y)))  
    empty                                =    MLens (\() -> undefined) snd



---------------------------------------------------------------------------------

-- Reusing types from the Vanilla Lens construction, to illustrate an example


data Atom                     =   Atom

data Atom'                     =   Atom'

newtype Composite a           = Composite a

data Molecule                 = Molecule

newtype NewComposite b        = NewComposite b

-- Let's define a function that acts as a magnifying glass, but we can use our comonad function
peep         ::   Composite Atom                ->   Atom
peep                    = undefined

-- Let's define a function that can assemble new types and create new ones from parts
comp         ::   (Molecule, Composite Atom)    ->   NewComposite Molecule
comp                    = undefined

-- This contravariant function will supply our original structure
preTreat     ::   m                              ->  Composite Atom
preTreat                = undefined

-- This covariant function will absorb our resultant type structures and possibly modify them further
postTreat    ::   NewComposite Molecule          ->   n
postTreat               = undefined


---------------------------------------------------------------------------------

-- Let's now make a concrete profunctor
ourLensP :: MonoLens Atom Molecule s t
ourLensP =   MLens  (peep . preTreat) (\z  -> postTreat . comp $ (fst z, preTreat $ snd z))

-- Now for the monoidal profunctor (seems to mimic the Cartesian Lens's embellished profunctor)
monoLensP :: MonoLens Atom Molecule (Composite Atom,t) (NewComposite Molecule, t)
monoLensP  =  MLens (peep . fst) (\y -> (comp (fst y, (fst . snd $ y)), (snd . snd $ y)))
                                 

-- We arrive an eerily similar Optic, when one compares it to the Cartestian Lens Optic 
monoLensOptical   ::   MonoLens Atom Molecule Atom Molecule   ->  MonoLens Atom Molecule (Composite Atom, Atom) (NewComposite Molecule, Atom) 
monoLensOptical   w   =   MLens (\x -> blik  (par w w)  ((peep . fst $ x), snd x))  (\y -> (comp (fst y, fst . snd $ y), snd . snd $ y))

---------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

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

-- Reusing types

-- Let's define some type from which we can compose things of type s
data Atom                     =   Atom
-- Let's define another type from which we can compose things of type s'
data Atom'                     =   Atom'

-- We'll do this for the other types as well (compared to the vanilla lens example function)

-- Another definition that can represent original whole structured types
newtype Composite a           = Composite a
newtype Composite' a'          = Composite' a'


-- Some type we can use to build up new structured types
data Molecule                 = Molecule
data Molecule'                = Molecule'


--Finally some new structure we can build up
newtype NewComposite b        = NewComposite b
newtype NewComposite' b'       = NewComposite' b'

-- Let's define a function that acts as a magnifying glass, but we can use our comonad function
peep         ::   Composite Atom                ->   Atom
peep                    = undefined

-- Let's define a function that acts as a magnifying glass, but we can use our comonad function
peep'         ::   Composite' Atom'                ->   Atom'
peep'                    = undefined

-- Let's define a function that can assemble new types and create new ones from parts
comp         ::   (Molecule, Composite Atom)    ->   NewComposite Molecule
comp                    = undefined

comp'         ::   (Molecule', Composite' Atom')    ->   NewComposite' Molecule'
comp'                    = undefined

-- This contravariant function will supply our original structure
preTreat     ::   m                              ->  Composite Atom
preTreat                = undefined

preTreat'     ::   m'                              ->  Composite' Atom'
preTreat'                = undefined

-- This covariant function will absorb our resultant type structures and possibly modify them further
postTreat    ::   NewComposite Molecule          ->   n
postTreat               = undefined

postTreat'    ::   NewComposite' Molecule'          ->   n'
postTreat'               = undefined

---------------------------------------------------------------------------------

-- Let's now make a  concrete profunctor
ourLensP :: MonoLens Atom Molecule Atom Molecule
ourLensP =   MLens  (peep . preTreat) (\z  -> postTreat . comp $ (fst z, preTreat $ snd z))

-- We make another for the first monoidal profunctor
monoLensP :: MonoLens Atom Molecule (s, s') (t, t')
monoLensP  =  MLens _ undefined


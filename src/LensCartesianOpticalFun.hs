module LensCartesianOpticalFun where 



import Control.Lens.Combinators    (Profunctor, dimap            )
import Data.Profunctor.Strong      (Strong    , first', second'  )

{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up


class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b                 <<-----
                                                             |-------- Either implement these two ...
  rmap  :: (b -> d) -> p a b -> p a d                 <<-----  

  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d     <<-------------- Or just this one


class Profunctor p => Strong p where
  first'  ::  p  a  b   -> p  (a,  c)  (b,  c)
   
  second' ::  p  a  b   -> p  (c,  a)  (c,  b)


where p is a Profunctor : 
type  Optic  p  a  b  s  t    =  p  a  b    ->   p  s  t

          

--}



---------------------------------------------------------------------------------

-- Popping up the definition from before
data  StrongLens  a  b  s  t          =    SLens    {   see    ::  s       ->   a,

                                                        update ::  (b, s)  ->   t
                                                    }



-- Making it a Profunctor first like we did before...
instance Profunctor (StrongLens a b) where
    dimap  h  g   (SLens v  w)        =    SLens   (v . h  ) (\x  ->  g $ w (fst x, (h $ snd x)))



-- We can make our Lens Cartesian now. 
--     Explanation :: -------->>>> For first' : binding the see function with the first tuple type would give us our required composite type (m . fst)
--                                            : (1) : the first tuple type represents the new types from which we can create new composites (b)
--                                            : (2) : we search within the second tuple entry for our reference type from which we can build new composites (fst . snd $ x)
--                                            : take (1) and (2) above as a tuple and : 
--                                                 --->> we supply the update function with this tuple to obtain our new composite types   (n . ((,) (fst x)) $ (fst . snd $ x))
--                                                 --->> make sure to associate that new composite type with the passthrough type (c) from the function input, in a tuple : (new_composite, (snd . snd $ x ))
--                    -------->>>> For second' : Flip the tuple order we obtained in first' ; also, the original composite type (s) is in the second position, which the passthrough type (c) is the in second position                      
instance Strong (StrongLens a b) where
    first'       (SLens  m   n)       =    SLens   (m . fst) (\x ->   (,)  (n   . ((,) (fst x)) $ (fst . snd $ x)) (snd . snd $ x ))
    second'      (SLens  m   n)       =    SLens   (m . snd) (\x ->   (,)  (fst . snd $ x) (n . ((,) (fst x)) $ (snd . snd $ x   )))




---------------------------------------------------------------------------------

-- Revisiting the types from the Vanilla Lens to see how things change with strength

data Atom                     =   Atom


newtype Composite a           =   Composite a


data Molecule                 =   Molecule


newtype NewComposite b        =   NewComposite b



---------------------------------------------------------------------------------
-- The functions from before....

preTreat     ::   m                              ->  Composite Atom
preTreat                      = undefined


postTreat    ::   NewComposite Molecule          ->   n
postTreat                     = undefined


peep         ::   Composite Atom                 ->   Atom
peep                          = undefined


comp         ::   (Molecule, Composite Atom)     ->   NewComposite Molecule
comp                          = undefined


---------------------------------------------------------------------------------


-- Ok, the profunctor we can form really hasn't changed at all
telescopicP :: StrongLens Atom Molecule Atom Molecule 
telescopicP  = SLens (peep . preTreat) (\z  -> postTreat . comp $ (fst z, preTreat $ snd z))


-- Fine, let's strengthen what we already have then, first in one way...
leftTelescopicP :: StrongLens Atom Molecule (Atom, d) (Molecule, d) 
leftTelescopicP  = first' telescopicP



-- then in the other way....
rightTelescopicP :: StrongLens Atom Molecule (d, Atom) (d, Molecule) 
rightTelescopicP  = second' telescopicP


---------------------------------------------------------------------------------

-- Let's create a Left Optical
leftOptical   ::  StrongLens Atom Molecule (Atom, d) (Molecule, d)   ->   StrongLens Atom Molecule (Composite Atom, d) (NewComposite Molecule, d) 
leftOptical   = undefined



--- try this : interesting   ::  StrongLens Atom Molecule (Atom, d) (Molecule, d)   ->    StrongLens Atom Molecule (d, Composite Atom) (d, NewComposite Molecule) 
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
data  StrongLens  a  b  s  t               =     SLens    {   see    ::  s       ->   a,

                                                              update ::  (b, s)  ->   t
                                                          }



-- Making it a Profunctor first like we did before...
instance Profunctor (StrongLens a b) where
    dimap  h   g    (SLens v  w    )        =    SLens   ( v . h ) (\x  ->  g $ w (fst x, (h $ snd x)))



-- We can make our Lens Cartesian now. 
--     Explanation :: -------->>>> For first' : binding the see function with the first tuple type would give us our required composite type (m . fst)
--                                            : (1) : the first tuple type represents the new types from which we can create new composites (b)
--                                            : (2) : we search within the second tuple entry for our reference type from which we can build new composites (fst . snd $ x)
--                                            : take (1) and (2) above as a tuple and : 
--                                                 --->> we supply the update function with this tuple to obtain our new composite types   (n . ((,) (fst x)) $ (fst . snd $ x))
--                                                 --->> make sure to associate that new composite type with the passthrough type (c) from the function input, in a tuple : (new_composite, (snd . snd $ x ))
--                    -------->>>> For second' : Flip the tuple order we obtained in first' ; also, the original composite type (s) is in the second position, which the passthrough type (c) is the in second position                      
instance Strong (StrongLens a b) where
    first'       (SLens  m   n)       =    SLens   (m . fst) (\x ->   (,)  (n   . ((,) (fst x)) $ (fst . snd $ x)) (snd . snd $ x  ))
    second'      (SLens  m   n)       =    SLens   (m . snd) (\x ->   (,)  (fst . snd $ x) (n . ((,) (fst x))  $   (snd . snd $ x )))




---------------------------------------------------------------------------------

-- Revisiting the types from the Vanilla Lens to see how things change with strength

data     Atom                     =   Atom


newtype  Composite    a           =   Composite    a


data     Molecule                 =   Molecule


newtype  NewComposite b           =   NewComposite b



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
telescopicP            = SLens (peep . preTreat) (\z  -> postTreat . comp $ (fst z, preTreat $ snd z))




-- Using just the convenience functions directly, we can form the First Profunctor......                                                             
firstTelescopic   ::   StrongLens Atom Molecule (Composite Atom, d) (NewComposite Molecule, d) 
firstTelescopic         = SLens (peep . fst) (\y -> (comp (fst y, fst . snd $ y), snd . snd $ y))



-- And for the Second Profunctor .....
secondTelescopic   ::   StrongLens Atom Molecule (d, Composite Atom) (d, NewComposite Molecule) 
secondTelescopic        = SLens (peep . snd) (\y -> (fst . snd $ y, comp (fst y, snd . snd $ y)))


-- Or we could take the scenic route, using an Optical definition below .....

---------------------------------------------------------------------------------

-- Now for the Optics....

-- We are taking a vanilla profunctor and not only left-strengthening it, but also transforming between composite types, rather than just between types that make up those composites
--    Explanation :  ------>>>>>  The see function : We will be transforming things from :  (s  -> a)   to             :  ((s, c)    ->  a)
--                                                 : What does that mean exactly? It means instead of going like this  :  (Atom  ->   Atom) , it will be :    ((Composite Atom, d)  -> Atom)
--                                                 : So, our new  input is really                                      :  ((Composite Atom, d)  -> Atom)
--                                                 : How can we use the input profunctor ?   Well, if we left-strengthened it and asked for its view function, then we can compose    :    ((Atom, w)  ->  Atom),  where w is some type....
--                                                 : Now, x gives us access to both input types from the above function (with w); just assume w is identical to c; So it is just a matter of supplying : (see (first' k)) with its parameters.....
--                   ------>>>>>  The update func  : We can't really use k, because we do not have a way to tranform   :    (Atom                   ->  Composite Atom)        <<<---- at least, not in any of the above definitions....
--                                                 :       If we use (update k)            :  The transformation is    :    ((Molecule, Atom)       ->   Molecule     )        <<<---- not useful.....
--                                                 :       If we use (update . first' k)   :  The transformation is    :    ((Molecule, (Atom, d))  ->   (Molecule, d))        <<<---- still dealing with Atoms....
--                                                 : But comp, by definition, seems eager to take parts of the input and provide the required types (NewComposite Molecule), we just need to include the pass-through type as well (d) ...                                          
firstOptical'   ::   StrongLens Atom Molecule Atom Molecule            ->     StrongLens Atom Molecule (Composite Atom, d) (NewComposite Molecule, d) 
firstOptical'   k       =     SLens (\x -> see  (first' k)  ((peep . fst $ x), snd x))  (\y -> (comp (fst y, fst . snd $ y), snd . snd $ y))





-- Same logic applies except now the pass-through type is in the first position....., the update function is just reverse order
secondOptical   ::   StrongLens Atom Molecule Atom Molecule            ->    StrongLens Atom Molecule (d, Composite Atom) (d, NewComposite Molecule) 
secondOptical   k       =     SLens (\x -> see   (second' k) (fst x, peep . snd $ x)  )  (\y -> (fst . snd $ y, comp (fst y, snd . snd $ y)))




-- One can also use a First-Strengthener to build an Optic that Second-Strengthens....
secondOptical'  ::   StrongLens Atom Molecule Atom Molecule            ->    StrongLens Atom Molecule (d, Composite Atom) (d, NewComposite Molecule) 
secondOptical'  k       =     SLens (\x -> see   (first' k) ((peep . snd $ x), fst x) )  (\y -> (fst . snd $ y, comp (fst y, snd . snd $ y)))




-- Adding this for more fun. Second' could also have been used....
flippyOptical   ::  StrongLens Atom Molecule (Atom, d) (Molecule, d)   ->   StrongLens Atom Molecule (d, Composite Atom) (d, NewComposite Molecule) 
flippyOptical    k     =     SLens  (\x  -> see  (second' k) (fst x, ((peep . snd $ x), fst x)))  (\y ->   ((fst . snd $ y), comp ((fst y), snd . snd $ y)))


---------------------------------------------------------------------------------

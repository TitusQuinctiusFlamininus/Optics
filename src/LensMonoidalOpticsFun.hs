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


-- Raising an earlier lens definition
data  MonoLens  a  b  s  t     =     MLens    {   blik   ::  s       ->   a,

                                                   upp   ::  (b, s)  ->   t
                                              }


-- The profunctor Lens as it was in Vanilla form.... 
instance Profunctor (MonoLens a b) where
        dimap  h  g  (MLens r  z)        =    MLens   ( r . h ) (\x  ->  g $ z (fst x, (h $ snd x)))



-- Now, attempting to make the Lens Monoidal...
--     Explanation :: ----->>>> Par Function:  - Left-Side: - Suppose, for par, we have these two hypothetical types: (MonoLens a b a1 b1) and (MonoLens a b c d)
--                                                          - We need a function that maps: (a1, c) -> a . This is the equivalent of: (s, c) -> a
--                                                          - To solve the left-side, we just need to produce an 'a' type: 
--                                                          - So by providing the "blik", or k-function, with the first element of the tuple, we're done.
--                    - Right-Side:    - Continuing with the two hypothetical types: (MonoLens a b a1 b1) and (MonoLens a b c d)  
--                                     - We need a function that maps: (b, (a1, c)) -> (b1, d) 
--                                     - First, we can notice that : a1 <=> s               (since: (MonoLens a b a1 b1) <=> (MonoLens  a b s t)
--                                     -        we can notice that :  c <=> s  and d <=> t  (since: (MonoLens a b c d)   <=> (MonoLens  a b s t)
--                                     - So let's simplify : (b, (a1, c)) -> (b1, d)  to  : (b, (s, c)) -> (t, t)   [we leave c as it is for better visuals]
--                                     - We just need to produce two 't' types, hopefully using the rest of the type functions, m and r
--                                     - (m (fst y, fst . snd $ y))  helps produce the first tuple 't' using out m function
--                                     - (r (fst y, snd . snd $ y))  helps produce the second tuple 't' using out r function
--                                     - Notice, finally, that we have no need for our view function from the second profunctor lens

--                   ----->>>> Empty Function:  - We require a Profunctor of the form: p () ()   :  which translates to : MonoLens  a  b  () () 
--                                              - Our "blik" function now becomes:  blik ::     ()   ->   a 
--                                              - Our "upp"  function now becomes:  upp  :: (b, ())  ->   ()
--                                              - Tackling the "upp" function first: All we need to do it access the second value in our input tuple: Done.
--                                              - The "blik" function is trickier: We need to produce some type a from unit.......but there is no known way to do that.
--                                              - To satisfy the typechecker, we leave the function undefined
instance Monoidal (MonoLens a b) where
    par (MLens k  m) (MLens _  r)        =    MLens (k . fst) (\y -> ((m (fst y, fst . snd $ y)) , r (fst y, snd . snd $ y))) 
    empty                                =    MLens (\() -> undefined) snd


---------------------------------------------------------------------------------

-- Reusing types from the Vanilla Lens construction, to illustrate an example


data    Atom                  =   Atom

newtype Composite     a       =   Composite    a

data    Molecule              =   Molecule

newtype NewComposite  b       =   NewComposite b


--And the follow-up functions to accompany these types...
peep         ::   Composite Atom                 ->   Atom
peep                    = undefined

comp         ::   (Molecule, Composite Atom)     ->   NewComposite Molecule
comp                    = undefined

preTreat     ::   m                              ->  Composite Atom
preTreat                = undefined

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
monoLensOptical   ::   MonoLens Atom Molecule Atom b   ->  MonoLens Atom Molecule (Composite Atom, Atom) (NewComposite Molecule, Atom) 
monoLensOptical   w   =   MLens (\x -> blik  (par w w)  ((peep . fst $ x), snd x))  (\y -> (comp (fst y, fst . snd $ y), snd . snd $ y))


---------------------------------------------------------------------------------

module LensInPhantomFun where


import Control.Lens.Combinators    (Profunctor, dimap            )

{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up


class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b                 <<-----
                                                             |-------- Either implement these two ...
  rmap  :: (b -> d) -> p a b -> p a d                 <<-----  

  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d     <<-------------- Or just this one


where p is a Profunctor : 

type  Optic  p  a  b  s  t    =  p  a  b    ->   p  s  t


class Choice p => InPhantom p where
    icoerce   ::   p  a  c   ->    p  b  c

    
--}


-- From the definition of InPhantom, it seems that we cannot create an InPhantomLens. 
-- The reason us that there is a typeclass contextual requrement where the Lens Profunctor has to be Choice (or CoCartesian)
-- We cannot create a CoCartesian Lens (see LensCoCartesianFun). 
-- Therefore, we cannot create a Lens that is also InPhantom.


---------------------------------------------------------------------------------
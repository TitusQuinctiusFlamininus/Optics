module TFunctor.LensOpticsFun where


import Control.Lens.Combinators (Profunctor, dimap)

{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

--First we define a regular Profunctor 
class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b                 <<-----
                                                             |-------- Either implement these two ...
  rmap  :: (b -> d) -> p a b -> p a d                 <<-----  

  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d     <<-------------- Or just this one

-- Next we express an Optic: 
type Optic p a b s t = p a b -> p s t


--Next we define a Tfunctor 
class TFunctor t' where
  tmap :: (c -> a) -> (b -> d) -> p a b -> p c d

--}



---------------------------------------------------------------------------------

-- Let's roll our own Lens
data TFunctorLens a b s t         = TLens {  look :: s         ->     a, 

                                             edit :: (b, s)    ->     t 
                                           }


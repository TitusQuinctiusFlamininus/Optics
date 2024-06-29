module PrismCartesianFun where


import Control.Lens.Combinators    ( Profunctor, dimap )

---------------------------------------------------------------------------------

 {--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up


class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b                 <<-----
                                                             |-------- Either implement these two ...
  rmap  :: (b -> d) -> p a b -> p a d                 <<-----  

  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d     <<-------------- Or just this one



where p is a Profunctor : 
type   Optic  p  a  b  s  t   =   p  a  b   ->  p  s  t


class Profunctor p => Strong p where
  first'  ::  p  a  b   -> p  (a,  c)  (b,  c)
   
  second' ::  p  a  b   -> p  (c,  a)  (c,  b)


--}   


---------------------------------------------------------------------------------
   
-- Defining a Strong Profunctor
class Profunctor p => Strong p where
  first'  ::  p  a  b   -> p  (a,  c)  (b,  c)
   
  second' ::  p  a  b   -> p  (c,  a)  (c,  b)



-- Our Prism Definition
data Prism a b s t     = Prism {    seek    :: s     ->     Either b a, 

                                    fill    :: b     ->     t
                               }

-- Make the Prism a Profuntor
-- Creating the Prism Profunctor as before....
-- If how this Profunctor was formed is not clear, please see the PrismOpticsFun module for a full explanation
-- I have used different type symbols here but it is essentially the same (just needs some eye squinting)
instance Profunctor (Prism s t) where 
    dimap  d  g  (Prism k  m)   =    Prism  (k . d)  (g . m)

-- And now attempting to Strengthen the Polyhedron...

instance Strong (Prism s t) where 
    first'   (Prism k  m)       =    Prism undefined undefined
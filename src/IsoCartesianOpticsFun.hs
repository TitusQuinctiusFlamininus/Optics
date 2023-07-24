module IsoCartesianOpticsFun where 



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


-- Defining the Isomorphism structure as a prerequisite
data  Iso  a  b  s  t                =     Iso    {   hin    ::  a       ->   b,

                                                      her    ::  s       ->   t
                                                    
                                                  }


-- Making the Iso a Profunctor
-- Explanation :   ------>>>>>   hin function :: No modifications needed, because no type variables appear for the instance requirement
--                               her function ::Straight Composition from the input of the contravariant function to the covariant one, through 'her'
instance  Profunctor (Iso a  b  )   where
    dimap    h    g  (Iso f  k  )       =    Iso  f    (g . k . h)


instance  Strong     (Iso a  b  )   where
    first'           (Iso f  k  )       =    Iso   f   (\x ->   ((k . fst $ x), snd   x  ))
    second'          (Iso f  k  )       =    Iso   f   (\x ->   (fst x,    (k . snd $ x )))
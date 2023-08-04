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
    dimap    h    g  (Iso f  k  )       =    Iso   f   (g . k . h)


instance  Strong     (Iso a  b  )   where
    first'           (Iso f  k  )       =    Iso   f   (\x ->   ((k . fst $ x), snd   x  ))
    second'          (Iso f  k  )       =    Iso   f   (\x ->   (fst x,    (k . snd $ x )))


 ---------------------------------------------------------------------------------

-- Lets re-use the types we invented in the Adapter, since isomorphisms are somewhat similar to adaptations
-- I will just restate them here for convenience


-- This was defined as a foundational element of some kind
data Raw             = Raw


-- And this type represented some form of maturity
data Ripe            = Ripe


-- This is a type representing the old guard
data Old             = Old 


-- And here is the new world order, so to speak
data New             = New


---------------------------------------------------------------------------------                       


-- Also re-stating previously defined functions for the AdapterOptic, for convenience

-- We perform this function when we are preparing a profunctor isomorphic transformation
preAdapt     :: s'          ->         Old
preAdapt                   = undefined


-- We perform this function when we are winding down the transformation
postAdapt    :: New         ->         t'
postAdapt                  = undefined


-- This is a function that deals with transforming base types into other base types
adapt        :: Raw         ->         Ripe
adapt                      = undefined


-- And finally this function has the ability to transform ancient types into more modern ones
unAdapt      :: Old         ->         New
unAdapt                    = undefined


---------------------------------------------------------------------------------
module TraversingLensOpticsFun where 



import Control.Lens.Combinators    (Profunctor, dimap               )
import Data.Profunctor.Traversing  (Traversing, traverse', wander   )
import Data.Profunctor.Strong      (Strong    , first',    second'  )
import Data.Profunctor.Choice      (Choice    , left' ,    right'   )

{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up


class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b                 <<-----
                                                             |-------- Either implement these two ...
  rmap  :: (b -> d) -> p a b -> p a d                 <<-----  

  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d     <<-------------- Or just this one


-- CoCartesian
class Profunctor p  =>  Cocartesian p where
  left'      ::  p  a  b   ->  p  (Either  a  c)  (Either  b  c)
  right'     ::  p  a  b   ->  p  (Either  c  a)  (Either  c  b)

--Cartesian 
class Profunctor p => Strong p where
  first'     ::  p  a  b   -> p  (a,  c)  (b,  c)
  second'    ::  p  a  b   -> p  (c,  a)  (c,  b)


class (Choice p, Strong p) => Traversing p where
  traverse'  :: Traversable f => p a b -> p (f a) (f b)
  wander     :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a b -> p s t


where p is a Profunctor : 
type  Optic  p  a  b  s  t    =  p  a  b    ->   p  s  t


--}



---------------------------------------------------------------------------------

-- First defining 
data  TravLens  a  b  s  t          =    TLens    {    seek    ::  s       ->   a,

                                                       mod     ::  (b, s)  ->   t
                                                  }



-- Again, our Vanilla Profunctor....
instance Profunctor (TravLens a b) where
    dimap  h  g   (TLens q  r)        =    TLens   ( q . h ) (\y  ->  g $ r (fst y, (h $ snd y)))

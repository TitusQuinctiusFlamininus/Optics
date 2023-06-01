module LensCoCartesianOpticalFun where 



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


class Profunctor p  =>  Cocartesian p where
  left'    ::  p  a  b   ->  p  (Either  a  c)  (Either  b  c)
  
  right'   ::  p  a  b   ->  p  (Either  c  a)  (Either  c  b)


where p is a Profunctor : 
type  Optic  p  a  b  s  t    =  p  a  b    ->   p  s  t


--}



---------------------------------------------------------------------------------

-- Exposing the Vanilla definition
data  ChoiceLens  a  b  s  t          =    Lens    {    seek    ::  s       ->   a,

                                                        mod     ::  (b, s)  ->   t
                                                    }



-- Again, our Vanilla Profunctor....
instance Profunctor (ChoiceLens a b) where
    dimap  h  g   (Lens q  r)        =    Lens   ( q . h ) (\y  ->  g $ r (fst y, (h $ snd y)))


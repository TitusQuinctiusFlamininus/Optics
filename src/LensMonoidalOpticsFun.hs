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

-- Popping up the definition from before
data  MonoLens  a  b  s  t     =     MLens    {   blik   ::  s       ->   a,

                                                   upp   ::  (b, s)  ->   t
                                              }



-- Making it a Profunctor first like we did before...
instance Profunctor (MonoLens a b) where
    dimap  h   g    (MLens r  z    )    =    MLens   ( r . h ) (\x  ->  g $ z (fst x, (h $ snd x)))



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


-- Raising the lens definition
data  MonoLens  a  b  s  t     =     MLens    {   blik   ::  s       ->   a,

                                                   upp   ::  (b, s)  ->   t
                                              }


-- First establishing the Profunctor...
instance Profunctor (MonoLens a b) where
        dimap  h  g  (MLens r  z)        =    MLens   ( r . h ) (\x  ->  g $ z (fst x, (h $ snd x)))


-- Now, attempting to make the Lens Monoidal...
instance Monoidal (MonoLens a b) where
    par (MLens k  m) (MLens _  r)        =    MLens (k . fst) (\y -> ((m (fst y, fst . snd $ y)) , r (fst y, snd . snd $ y)))  
    --empty                              =    ??
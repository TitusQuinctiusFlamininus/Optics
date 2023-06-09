module LensClosedOpticsFun where


import Control.Lens.Combinators (Profunctor, dimap)

{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up


class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b                 <<-----
                                                             |-------- Either implement these two ...
  rmap  :: (b -> d) -> p a b -> p a d                 <<-----  

  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d     <<-------------- Or just this one


Where p is a Profunctor : 
type Optic p a b s t   =      p a b    -> p s t


class Profunctor p => Closed p where    
    closed :: p a b    ->     p (x -> a) (x -> b)    

--}



---------------------------------------------------------------------------------


-- Reminding ourselves of the Lens definition
data SealLens a b s t              =      SLens   {    ret   :: s         ->     a, 

                                                       fix   :: (b, s)    ->     t 
                                                  }


-- Now quickly making a Profunctor Lens, since we've done it before....
instance Profunctor (SealLens a b) where
    dimap  h  g   (SLens  f   k)   =      SLens (f  . h) (\x -> g $ k (fst x, h . snd $ x))



class Profunctor p => Closed p where    
    closed :: p a b    ->     p (x -> a) (x -> b)  


-- Cant be done, because we cannot resolve x: it could be anything
--instance Closed (SealLens a b)  where
--    closed  (SLens  f   k)         =      SLens  _ undefined
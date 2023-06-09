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


-- Let's define what is meant by a Closed Profunctor
-- According to the Profunctors documentation : 
--  --->> A Strong profunctor allows the monoidal structure to pass through, but : 
--  --->> A Closed profunctor allows the closed structure to pass through.
class Profunctor p => Closed p where    
    closed :: p a b    ->     p (x -> a) (x -> b)  



-- Reminding ourselves of the Lens definition
data SealLens a b s t              =      SLens   {    ret   :: s         ->     a, 

                                                       fix   :: (b, s)    ->     t 
                                                  }



-- Now quickly making a Profunctor Lens, since we've done it before....
instance Profunctor (SealLens a b) where
    dimap  h  g   (SLens  f   k)   =      SLens (f  . h) (\x -> g $ k (fst x, h . snd $ x))



-- And now attempting to make it Closed: 
-- Seems we cannot. 
--   --->>> EXPLANATION 
--          For ret     :  The transformation is from :      (s  ->  a)   to  :  ((x -> s) -> a)       (And for simplicity, let y :: (x -> s))
--                      :  The aim is to produce something of type a, but to do that, we need to first create types (s) to give to f : (\y -> f (y $ ??))
--                      :  We need things of type x to complete the process, but have no access to these types (and we cannot create them from other types or functions)
--          For fix     :  The transformation is from :     ((b,s) -> t)  to  :  ((b, (x -> s)) -> (x -> t))
--                      :  The idea is similar to ret : use k to create types t, then resolve other types, but we need to create the bare tuple (b, s) first
--                      :  We need to provide the second tuple value with type x, and provide the result to k.......but we need type x for that! 
--instance Closed (SealLens a b)  where
--    closed  (SLens  f   k)         =      SLens  (\y -> f (y $ ??)) (\y' ->  (\?? -> k (fst y', ((snd y') $ ??))))



---------------------------------------------------------------------------------

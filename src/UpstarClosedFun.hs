
module UpstarClosedFun where


import Control.Lens.Combinators (Profunctor, dimap)

{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap          ::  (c -> a)   -> p a b    -> p c b                      <<-----
                                                                               |-------- Either implement these two ...
  rmap          ::  (b -> d)   -> p a b    -> p a d                      <<-----  

  dimap         ::  (c -> a)   -> (b -> d) -> p a b         -> p c d     <<-------------- Or just this one


--}


---------------------------------------------------------------------------------

-- Let's define what is meant by a Closed Profunctor
-- According to the Profunctors documentation : 
--  --->> A Strong profunctor allows the monoidal structure to pass through, but : 
--  --->> A Closed profunctor allows the closed structure to pass through.
class Profunctor p => Closed p where    
    closed     :: p a b    ->     p (x -> a) (x -> b)    



-- Ok, Let us invent our own Upstar...
newtype ClosedStar f  a  b             =   ClosedUpstar { seal  ::   a   ->    f b }



-- The profunctor is not hard to invent...
instance Functor f => Profunctor (ClosedStar f)    where
    dimap  h   g   (ClosedUpstar k)    =   ClosedUpstar (fmap g . k . h)



-- And now attempting to make it Closed: 
-- Seems we cannot. 
--   --->>> EXPLANATION 
--                    :  The transformation is from : (a  ->  f b)   to  :  ((x -> a) -> f (x -> b))
--                    :  Let's suppose we can produce (f b) . It means that we will need to "fmap" that to some function
--                    :  So far, we have   :   (\y ->  fmap ??  (f b)  ), where  y :: (x -> a)
--                    :  The ?? turns out to be a function that takes type b and produces the function we need (x -> b)
--                    :  In other words, we need a function like this :   (\b  ->  (x -> b))
--                    :                  ------->>>> Which is really  :   (\b  ->  x  ->  b), which looks like "const" (from FP basics)
--                    :  Now the only problem is to create (f b). We need to give k something of type a; 
--                    :         We do not have any means to create or obtain structures of type a. End of Exercise

--instance Functor f => Closed (ClosedStar f)  where
--    closed (ClosedUpstar k)    =   ClosedUpstar (\y -> fmap const (k $ ??))



---------------------------------------------------------------------------------

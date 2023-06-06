module DownstarInPhantomFun where 


import Control.Lens.Combinators    ( Profunctor, dimap )

{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up


class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b                 <<-----
                                                             |-------- Either implement these two ...
  rmap  :: (b -> d) -> p a b -> p a d                 <<-----  

  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d     <<-------------- Or just this one


where p is a Profunctor : 
type  Optic  p  a  b  s  t    =  p  a  b    ->   p  s  t


class Profunctor p  =>  Choice p where
  left'    ::  p  a  b   ->  p  (Either  a  c)  (Either  b  c)
  
  right'   ::  p  a  b   ->  p  (Either  c  a)  (Either  c  b)


class Choice p => InPhantom p where
    icoerce   ::   p  a  c   ->    p  b  c

--}


---------------------------------------------------------------------------------


-- Let's remind ourselves of the Downstar type
data      DownPhantom f  a  b            =      DownInPhantom   { vanish   ::   f a   ->   b  }



-- Let's make it a Profunctor
instance Functor f => Profunctor (DownPhantom f) where
    dimap  h  g   (DownInPhantom  w)     =      DownInPhantom   (     g  .  w  . fmap h     )


-- Attempting to make it an InPhantom : First we have to make our Downstar CoCartesian (Choice p => InPhantom p)
-- This is not feasible, since Downstar cannot be a Choice instance (see DownstarCoCartesian)
-- Therefore, inPhantom Downstars are unrealistic. 
 

---------------------------------------------------------------------------------

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


 
-- From the definition of InPhantom, it seems that we cannot create an InPhantom Downstar. 
-- The reason us that there is a typeclass contextual requirement where the Downstar Profunctor has to be Choice : (Choice p => InPhantom p)
-- But         : We cannot create a CoCartesian Downstar (see DownstarCoCartesianFun). 
-- Therefore   : We cannot create a Downstar that is also InPhantom.

---------------------------------------------------------------------------------

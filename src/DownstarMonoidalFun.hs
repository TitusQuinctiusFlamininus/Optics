module DownstarMonoidalFun where 


import Control.Lens.Combinators    (Profunctor, dimap)


 {--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b                 <<-----
                                                             |-------- Either implement these two ...
  rmap  :: (b -> d) -> p a b -> p a d                 <<-----  

  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d     <<-------------- Or just this one




class Profunctor p => Monoidal p where
  par         :: p a b    -> p c d    -> p (a, c) (b, d)
  empty       :: p () ()


class Functor f where
    <$>     :: (a -> b) -> f a -> f b


class functor f => Applicative f where
    pure      :: x           -> f x
    <*>       :: f (a -> b)  -> f a   ->  f b      

--}


---------------------------------------------------------------------------------


-- Refreshing our memory.....
class Profunctor  p => Monoidal p where
  par        ::   p a b     ->    p c d     ->     p (a, c) (b, d)
  empty      ::   p () ()



--Ok, then we define our own...
newtype DownMonoid  f  a  b                    =     LowMonoidal   {  down ::   f  a    ->   b  }



-- And roll up a Monoidal Profunctor for the Downstar ...
instance Functor f =>  Profunctor (DownMonoid f) where
    dimap   h   g    (LowMonoidal u)           =      LowMonoidal  (  g . u . fmap h )


-- Seems we must construct   :   down' ::  ( f (a, c)  -> (b, d) ) for the LHS equation construct :       par  (LowMonoidal u)  (LowMonoidal v) 
-- This instance seems impossible to construct : Why? 
-- Explanation : 
------ >>>>>>  For our par implementation :   
--             But soon we find out that on the tuple left :   (fmap (u . pure . fst) x)   :: f b     (but we need b, not (f b)
--             And we also find that on the tuple right    :   (fmap (v . pure . snd) x)   :: f d     (but we need d, not (f d)
--             Solution ?
--             If we can disassociate the types from their contexts, we'd be in business; that could be done be raising the abstraction of the f;
--             We will not tamper with the context definition (....instance Functor f =>  ....), but leave it as it is. 

------ >>>>>>  For our emtpy implementation :  
--             What about for empty? : Same problem of context disassociation  : We need  :  f () -> ()
--
--             instance Functor f  => Monoidal (DownMonoid f) where
--                     par      (LowMonoidal u)  (LowMonoidal v)  =   Not possible (UNLESS you raise the abstraction of f)
--                     empty                                      =   Not possible (UNLESS you raise the abstraction of f)



---------------------------------------------------------------------------------


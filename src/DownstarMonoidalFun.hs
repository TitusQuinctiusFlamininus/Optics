module DownstarMonoidalFun where 


import Control.Lens.Combinators    (Profunctor, dimap)


 {--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap    :: (c -> a) ->  p a b    -> p c b
  rmap    :: (b -> d) ->  p a b    -> p a d
  dimap   :: (c -> a) ->  (b -> d) -> p a b -> p c d


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
  par        ::   p a b     ->    p c d     -> p (a, c) (b, d)
  empty      ::   p () ()



--Ok, then we define our own...
newtype DownMonoid  f  a  b                    =     LowMonoidal   {  down ::   f  a    ->   b  }



-- And roll up a Monoidal Profunctor for the Downstar ...
instance Functor f =>  Profunctor (DownMonoid f) where
    dimap   h   g    (LowMonoidal u)          =      LowMonoidal  (  g . u . fmap h )

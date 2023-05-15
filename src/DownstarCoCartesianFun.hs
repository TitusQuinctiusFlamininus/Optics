module DownstarCoCartesianFun where



import Control.Lens.Combinators    (Profunctor, dimap)


 {--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap    :: (c -> a) ->  p a b    -> p c b
  rmap    :: (b -> d) ->  p a b    -> p a d
  dimap   :: (c -> a) ->  (b -> d) -> p a b -> p c d


class Profunctor p => Cocartesian p where
  left'    :: p a b   ->  p (Either a c) (Either b c)
  right'   :: p a b   ->  p (Either c a) (Either c b)


class Functor f where
    <$>     :: (a -> b) -> f a -> f b

--}


--Checking out the Downstar 
newtype CoStrong f a b               =    DownCoStar  {  low ::   f  a    ->   b  }


-- Making it a Profunctor is easy enough....
instance Functor f =>  Profunctor (CoStrong f) where
    dimap h g (DownCoStar u)          =    DownCoStar (  g . u . fmap h )

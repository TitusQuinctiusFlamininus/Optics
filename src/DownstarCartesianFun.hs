module DownstarCartesianFun where


import Control.Lens.Combinators    (Profunctor, dimap           )
import Data.Profunctor.Strong      (Strong    , first', second' )

 {--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap    :: (c -> a) ->  p a b    -> p c b
  rmap    :: (b -> d) ->  p a b    -> p a d
  dimap   :: (c -> a) ->  (b -> d) -> p a b -> p c d


class Profunctor p => Strong p where
  first'  ::  p a b   -> p (a, c) (b, c)
  second' ::  p a b   -> p (c, a) (c, b)


class Functor f where
    <$>     :: (a -> b) -> f a -> f b

--}


-- Revisiting the type we know by now
newtype CartesianDown   f  a  b      =   StrongDown {  low ::   f  a    ->   b  }


-- And revisiting how we would make this a Profunctor.....
instance Functor f => Profunctor (CartesianDown f) where
  dimap h g ( StrongDown d )         =   StrongDown (  g . d . fmap h )


-- Ok, so far so good. Now let's make it Strong
instance Functor f => Strong (CartesianDown f) where
    first'   ( StrongDown d )        =    StrongDown undefined
    second'  ( StrongDown d )        =    StrongDown undefined
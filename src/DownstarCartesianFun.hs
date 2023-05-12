module DownstarCartesianFun where


import Control.Lens.Combinators    (Profunctor, dimap)

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
{--

Why is it not possible to establish a Strong instance for Downstar?
----------->>>>>      First  : Instead of going like this:  (f a  ->  b) , we are going like this:  (f (a, c)   ->   (b, c))
--                    Second : Ok, so we have : f (a, c) as input; Let's fmap with a function that takes (a, c)
--                    Third  : If we take (a, c) and simply map to a, then the entire fmap computation would produce ; (f a); Ok so far.....
--                    Fourth : What can we do with (f a) ? We can apply d to this, giving us : b
--                    So it looks like this now                      :   (d .  fmap $ fst)
--                    Fifth  : We don't need just b, but : (b, c) ; But there is no way to bring a new type c, into scope! 
--                    We need the whole structure to look like this  :  ((d .  fmap $ fst), c)
--         Hence the typechecker complains : "Couldn't match expected type ‘(b, c)’ with actual type ‘b’". Game over.
--         The same applies to the second' cartesian function :(


instance Functor f => Strong (CartesianDown f) where
    first'   ( StrongDown d )        =    Not Possible 
    second'  ( StrongDown d )        =    Not Possible 

 --} 

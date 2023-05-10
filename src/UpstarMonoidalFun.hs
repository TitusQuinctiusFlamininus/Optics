module UpstarMonoidalFun where

import Control.Lens.Combinators (Profunctor, dimap)


{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap        :: (c -> a) -> p a b    -> p c b
  rmap        :: (b -> d) -> p a b    -> p a d
  dimap       :: (c -> a) -> (b -> d) -> p a b -> p c d


class Profunctor p => Monoidal p where
  par         :: p a b    -> p c d    -> p (a, c) (b, d)
  empty       :: p () ()


class Functor f where
    fmap      :: (a -> b) -> f a -> f b


class functor f => Applicative f where
    pure      :: x           -> f x
    <*>       :: f (a -> b)  -> f a   ->  f b

--}


-- Could not find a 'standard' library that contained this definition, so i'll just spell it out here for the typechecker

class Profunctor p => Monoidal p where
  par   :: p a b  -> p c d -> p (a, c) (b, d)
  empty :: p () ()



-- Ok, Let us invent our own Upstar
newtype MonoStar f  a  b                     =   MonoidalStar { unstar ::  a  ->  f b }



-- Alright, making it a Profunctor...
instance Functor f =>  Profunctor (MonoStar f) where
    dimap h g (MonoidalStar r)               =   MonoidalStar (fmap g . r . h) 


-- And now making it Monoidal...

-- Explanation  : 
--        ------->>> Take the first tuple type and apply it to v, we obtain a (f b);
--                   Then fmap that to a tuple function; we end up with a partial tuple function associated with a functor context : f (\some_type ->  (b, some_type))
--                   We can produce an (f d) by using the second profunctor, by applying the second tuple type to w;
--                   Now all we need to do it apply the two applicative functors together with the 'starship' function; we end up with a profunctor that maps between a tuple and tuple associated with an applicative context
--        ------->>> For empty, we have to end up with : f (),  so we need pure to associate unit with the functor
instance Applicative f =>  Monoidal   (MonoStar f) where
    par   (MonoidalStar v) (MonoidalStar w)  =   MonoidalStar (\x  ->  ((,) <$> (v . fst $ x)) <*>  (w . snd $ x))
    empty                                    =   MonoidalStar pure 
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

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


class Functor f where
    fmap        :: (a -> b)   -> f a      -> f b


class Functor g => Distributive g where
    distribute :: Functor f => f (g a)    -> g (f a)                     <<--------------  Implement this ...
    collect    :: Functor f => (a -> g b) -> f a     -> g (f b)          <<-------------- Or just this


class Profunctor p => Closed p where    
    closed     :: p a b    ->     p (x -> a) (x -> b)    

--}


---------------------------------------------------------------------------------

class Functor g => Distributive g where
    distribute :: Functor f => f (g a)    -> g (f a)                     
    collect    :: Functor f => (a -> g b) -> f a  -> g (f b)          

-- Defining what is meant by a Closed Profunctor
class Profunctor p => Closed p where    
    closed :: p a b    ->     p (x -> a) (x -> b)  

---------------------------------------------------------------------------------


-- Ok, Let us invent our own Upstar
newtype ClosedStar f  a  b             =   ClosedUpstar { seal  ::   a   ->    f b }



instance Functor f => Profunctor (ClosedStar f)    where
    dimap  h   g   (ClosedUpstar k)    =   ClosedUpstar (fmap g . k . h)


instance Functor f => Functor    (ClosedStar f a)  where
    fmap   f'      (ClosedUpstar k)    =   ClosedUpstar (fmap f' . k)



instance Functor f => Closed (ClosedStar f)  where
    closed (ClosedUpstar k)    =   ClosedUpstar (\x -> fmap const k)
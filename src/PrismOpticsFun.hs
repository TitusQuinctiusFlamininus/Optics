module PrismOpticsFun where


import Control.Lens.Combinators (Profunctor, dimap)
import Control.Comonad          (Comonad   , duplicate, extract, extend )

---------------------------------------------------------------------------------

 {--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up


class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b
  rmap  :: (b -> d) -> p a b -> p a d
  dimap :: (a1 -> b1) -> (c -> d) -> p b1 c -> p a1 d


where p is a Profunctor : 
type Optic p a b s t = p a b -> p s t


class Functor f where
    <$> :: (a -> b) -> f a -> f b

class Functor w => Comonad w where
    extract     ::  w a -> a      
    duplicate   ::  w a -> w (w a)
    extend      :: (w a -> b) -> w a -> w b    

--}   

---------------------------------------------------------------------------------
   
-- Rocking our very own Prism
data Polyhedron a b s t     = Poly {  peer    :: s  ->  Either b a, 
                                      pack    :: b  ->  t
                                   }


-- Making a profunctor out of the polyhedron, its pretty straight forward
instance Profunctor (Polyhedron s t) where 
     dimap h g (Poly l v)   = Poly (l . h)  (g . v)       



---------------------------------------------------------------------------------

-- Next we need types that we could use to illustrate prisms and optics related to prisms
data       Crystal           = Crystal


data       Shard             = Shard


newtype    Glass   a         = Glass   a


newtype    Diamond b         = Diamond b


---------------------------------------------------------------------------------

instance Functor Glass where
    fmap f (Glass x)          = Glass (f x)


instance Comonad Glass where
    extract (Glass x)         = x
    duplicate  x              =  Glass x
    extend     f              =  fmap f . duplicate



---------------------------------------------------------------------------------

-- Now we need functions that can deal with prisms


-- we need a function that will provide the materials to make a wonderful prism
preheat      :: a'           ->  Glass  a
preheat                       = undefined


-- Now we need something that will polish up our prism before we display it
cool         :: Diamond b'   ->         d
cool                          = undefined


-- This function will try and look inside some structure using the prism; prisms help us capture the idea of not finding what we are looking for
magnify      :: Glass  a     ->  Either e  a
magnify                       = undefined


-- This function can build a new structure from fragments of new material, using the prism
pressurize   :: b'           ->  Diamond b'
pressurize                    = undefined

---------------------------------------------------------------------------------

-- Making our Polyhedron into a Profunctor
hubble       :: Polyhedron Crystal Shard s t 
hubble                        = dimap preheat cool (Poly magnify pressurize)


-- Creating the Prismatic Optic
monocle      :: Polyhedron Crystal Shard Crystal Shard     ->    Polyhedron Crystal Shard (Glass Crystal) (Diamond Shard) 
monocle  (Poly ask _)   = Poly (ask . extract . preheat) pressurize   
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

class Functor r => Applicative r where
    pure        :: x           -> r x
    <*>         :: f (a -> b)  -> f a   ->  f b

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

-- We need Glass to be a Comonad so that we can handle whatever the contravariant 
-- function is providing internally to our Profunctor

-- So first we need to make it a functor....
instance Functor Glass where
    fmap f (Glass x)               = Glass (f x)



-- then we define the comonad instance
instance Comonad Glass where
    extract (Glass x)              = x
    duplicate  x                   =  Glass x
    extend     f                   =  fmap f . duplicate



-- We also want to show a difference in implementation later, so let's make another type a functor
instance Functor Diamond where
    fmap f (Diamond y)             = Diamond (f y)



-- Now let's make it an applicative
instance Applicative Diamond where
  pure x                           =  Diamond x
  Diamond f   <*>  Diamond t       =  Diamond (f t)

---------------------------------------------------------------------------------

-- Ok: we need practical functions that can deal with prisms


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



-- This function can build a new structures in a slightly different way
-- We compress by simply lifting the type into our Functor
compress     :: c            ->  Diamond c
compress                      = pure


---------------------------------------------------------------------------------


-- Making our Polyhedron into a Profunctor
hubble       :: Polyhedron Crystal Shard s t 
hubble                        = dimap preheat cool (Poly magnify pressurize)



-- Here's another kind of profunctor that forms the final type in the different way
webb         :: Polyhedron Crystal Shard s t 
webb                          = dimap preheat cool (Poly magnify compress  )



---------------------------------------------------------------------------------


-- Creating the Prismatic Optic
-- An interesting difference from our Profunctor LensOptic : This time we don't need the RHS transformation from our input, 
--    we will simply use some manner in which we form the new composite 
-- We can decide to pass in the final type formation function separately, or simply ignore how the profunctor provided forms the final type 

-- Simplifying
type Mirror a                         = Polyhedron Crystal Shard a Shard   ->  Polyhedron Crystal Shard (Glass a) (Diamond Shard) 



-- Let's fix it
monocleFixed      :: Mirror a 
monocleFixed     (Poly ask _)         = Poly (ask . extract) pressurize   



-- Or we can make it a little more flexible   
monocleFlexible   :: (Shard  ->  Diamond Shard)   -> Mirror a
monocleFlexible f                     = \k ->  Poly (peer k . extract . preheat) f



---------------------------------------------------------------------------------

-- Using the types to observe the universe!

-- Let's use the Hubble Telescope to look around space...
observeStarH :: (Glass Crystal)  ->  Either Shard Crystal
observeStarH        =    peer  (monocleFlexible pressurize hubble )



-- Now let's try to use the James T Webb Telescope to see if we can find a blackhole...
observeStarW :: (Glass Crystal)  ->  Either Shard Crystal
observeStarW        =    peer   (monocleFlexible compress webb    )



-- Or we can just use a standard way, regardless of the input
observeStar :: Polyhedron Crystal Shard a Shard     -> (Glass a)    ->  Either Shard Crystal
observeStar    p    =    peer   (monocleFixed p                   )



-- Finally let's form Shards in a Hubbly way
packH       :: Shard    ->    Diamond Shard
packH               =    pack   (monocleFixed hubble              )



-- And let'also form Shards in a Webby way
packW       :: Shard    ->    Diamond Shard
packW               =    pack   (monocleFixed webb                )



---------------------------------------------------------------------------------

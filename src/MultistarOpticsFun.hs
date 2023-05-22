module MultistarOpticsFun where

import Control.Lens.Combinators (Profunctor, dimap)
import Control.Comonad          (Comonad   , duplicate, extract, extend )

---------------------------------------------------------------------------------

 {--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up


class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b                 <<-----
                                                             |-------- Either implement these two ...
  rmap  :: (b -> d) -> p a b -> p a d                 <<-----  

  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d     <<-------------- Or just this one



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


-- Combination of an Upstar and Downstar (It is an experimental type, purely for fun)
data Multistar   f a b s t        = Multistar    {  up   ::   a     ->    f b,

                                                    down ::   f s   ->    t
                                                 }


-- Now we construct a Profunctor, like we always do
instance Functor f => Profunctor (Multistar f s t) where
    dimap h g (Multistar u d)     =    Multistar (u . id) (g . d . fmap h)

-- Explanation: 
    -- (For Multistar's left-hand-side  (L.H.S) function ): u takes something of type a ; let's assume h can produce that type; if so, h is really: (some_type_call_it_a' -> a): Why not just assume a' is identical to type a, then use identity? Easy enough
    -- (For Multistar's right-hand-side (R.H.S) function): 
                  --  Notice   first  : The left hand side (as explained above), goes from : (a -> f b) . The type a here is not necessarily identical to a' in h; it could be a', but it could also be something else
                  --           second : s and t are types that 'make up' the concept of the multistar profunctor ((p s t), according to the dimap syntax above), but they dont appear as types on the left, only on the right;  
                  --                    and our profunctor still needs to go from : (input_of_h -> output_of_g) : the right-hand-side will need to fully resolve this mapping somehow  
                  --  We need to go from some (input_of_h -> something) : so let's start with h : that takes care of this problem 
                  --  (d . h) makes no sense because d takes a functor of some type: So fmap is necessary : so now we have (d . fmap (a' -> ?)) : The ? turns out to be s : Why? Because u takes (f s) : So h is really : (a' -> s)
                  --  (d . fmap h) produces a type t: What consumes things of type t? loooks like g . So let's compose to get :  (g . d . fmap h)        
                  --  (g . d . fmap h)  seems to be of type : (input_of_h -> output_of_g), solving the type-mapping problem


---------------------------------------------------------------------------------

-- Let's now invent some practical types for our exemplified use


-- Here's a type that will behave like a functor later
newtype  SuperStar s        = Star s


-- Here's some arbitrary type that mimicks intersteller dirt
data     Dust               = Dust 


-- Here's another type that can be used as some intersteller dirt
data     StarDust           = StarDust


-- Some kind of composite type that could hold a form of space dirt
newtype  Cloud c            = Cloud c


-- Another intersteller composite type that holds a different type of space dirt 
newtype  Cluster v          = Cluster v


---------------------------------------------------------------------------------

-- We need one of our types to behave like a functor; so let's make it do that now

-- Making our custom type a functor
instance Functor SuperStar where
  fmap f  (Star n)          = Star (f n)



-- This instance will come in useful when we need a type associated with a functor, rather than the composition of the two
instance Comonad SuperStar where
    extract (Star x)        =  x
    duplicate  x            =  Star x
    extend     f            =  fmap f . duplicate



-- This instance will come in useful when we have to deal with input as a type, rather than a functor of some type
instance Applicative SuperStar where
  pure x                    =  Star x
  Star y   <*>  Star z      =  Star (y z)


---------------------------------------------------------------------------------

-- Time to form a concrete set of functions that we can utilize 
-- Analogy of star-formation in galaxies used, just as an example

-- We need a contravariant function for providing our intersteller material during Star formation
epoch      ::  a'           ->        s
epoch       = undefined



-- We also need a covariant function that will eventually produce the space phenomenon we seek
evolve     :: t              ->       d
evolve      = undefined



-- Let's invent a function that mimicks some process not really associated with star-formation (L.H.S Multistar)
outflow    :: a              ->       SuperStar b 
outflow     = undefined



-- Finally, inventing a function within which Stars die 
supernova  :: SuperStar s    ->       t
supernova   = undefined


---------------------------------------------------------------------------------


-- Formulating a praktisk profunctor
multiFunctor :: Multistar SuperStar a b s t
multiFunctor                    = dimap epoch evolve (Multistar outflow supernova)



-- Coming up with our Optical, so that we can transform between simple Dust to Intersteller Dust
-- Explanation: LHS : Since how the profunctor works is unaffected by the LHS, we can just re-use what we get without modification
--              RHS : first   : Right now, our type is : ( SuperStar Dust -> StarDust) ........ but we need: SuperStar (Cloud Dust) -> Cluster StarDust
--                    second  : Let's compare inputs from both :  (SuperStar Dust)  versus :  (SuperStar (Cloud Dust)) : Need a function with type: (Dust  -> Cloud Dust)
--                    third   : We could invent the aforementioned function, or take something that can work with ANY type, and assume we are producing the type we need  ------>>> that looks like h (or epoch), but we need to fmap first
--                    fourth  : Now that we have : (SuperStar Stardust), we extract to dissociate it from the functor 
--                    fifth   : We then hand it over to a function that with type : (Stardust -> Cluster StarDust) ....... but evolve goes from any-type to any-type : Perfect!
-- So our final signature is: Multistar SuperStar a b Dust StarDust  -> Multistar SuperStar a b (Cloud Dust) (Cluster StarDust)
multiOptic   :: (Comonad f) => Multistar f a b s' s       ->       Multistar f a b t t'
multiOptic (Multistar l _)      = Multistar l (extract . (<$>) epoch)

-- NOTE THIS ALSO WORKS AS AN IMPLEMENTATION  :  ----->>>> Multistar l (evolve . extract . (<$>) epoch)   <<<<-------



---------------------------------------------------------------------------------
-- Let's use this stuff!

-- Lifting Intersteller dust into our intersteller functor, using our Optic
-- We don't really mind what type we get as input and what functor we create, as far as the Profunctor is concerned
upper        :: c'             ->       SuperStar d'
upper                           = up   (multiOptic multiFunctor)


-- Transposing between Intersteller Composite types, using our Optic
-- What we really want it to be able to transform from one composite type to another composite (intersteller) type
-- We need pure to lift a type into a functor
downer        :: Cloud a       ->       Cluster b
downer                          = down (multiOptic multiFunctor) . pure



---------------------------------------------------------------------------------

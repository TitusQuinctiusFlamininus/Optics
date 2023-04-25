module MultistarOpticsFun where

import Control.Lens.Combinators (Profunctor, dimap   )

 {--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

Reminder of the types in question: 

newtype Upstar   f a b  = Upstar   { upStar   :: a   -> f b }
newtype Downstar f a b  = Downstar { downStar :: f a -> b   }

class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b
  rmap  :: (b -> d) -> p a b -> p a d
  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d

where p is a Profunctor : 
type Optic p a b s t = p a b -> p s t


class Functor f where
    fmap :: (a -> b) -> f a -> f b


--}   

-- Combination of an Upstar and Downstar
data Multistar   f a b s t        = Multistar    {  up   :: a     -> f b,
                                                    down :: f s   -> t
                                                 }


-- Now we construct a Profunctor, like we always do
instance Functor f => Profunctor (Multistar f a b) where
    dimap h g (Multistar u d)     =    Multistar (u . id) (g . d . fmap h)

-- Explanation: 
    -- (For Multistar's left-hand-side function ): u takes something of type a ; we know h must produce that type; so h is really: (some_type_call_it_a' -> a): Why not just assume a' is identical to a and use identity. Easy enough
    -- (For Multistar's right-hand-side function): 
    --            --  Notice first  : The left hand side (as explained above), goes from : (a -> f b) . The type a here is not necessarily identical to a' in h; it could be a', but it could also be something else
    --                       second : s and t are types that 'make up' the concept of a multistar profunctor here but they dont appear as types on the left, only on the right; 
    --                       third  : our profunctor still needs to go from : (input_of_h -> output_of_g) : the right-hand-side will need to fully resolve this mapping somehow 
                  --  We need to go from some (input_of_h -> something) : so let's start with h : that takes care of this problem 
                  --  (d . h) makes no sense because d takes a functor of some type: So fmap is necessary : so now we have (d . fmap (a' -> ?)) : The ? turns out to be s : Why? Because u takes (f s) : So h is really : (a' -> s)
                  --  (d . fmap h) produces a type t: What consumes things of type t? loooks like g . So we hand it the result, and we have :  (g . d . fmap h)        
                  --  (g . d . fmap h)  seems to be of type : (input_of_h -> output_of_g), solving the type-mapping problem

---------------------------------------------------------------------------------

-- Let's now invent some practical types for our exemplified use


-- Here's a type that will behave like a functor later
newtype  SuperStar s        = Star s


-- Here's some arbitrary ingredient type
data     StarDust           = StarDust 


-- Here's another arbitrary result type
data     Dust               = Dust


-- Here's a type that looks like it might contain other types
newtype  Cloud c            = Cloud c


-- This type can be formed by interchanging internal types for others
newtype  Cluster v          = Cluster v


---------------------------------------------------------------------------------

-- We need one of our types to behave like a functor; so let's make it do that now


instance Functor SuperStar where
  fmap f  (Star n)   = Star (f n)


---------------------------------------------------------------------------------


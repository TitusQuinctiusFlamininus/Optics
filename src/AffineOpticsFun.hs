module AffineOpticsFun where

 
import Control.Lens.Combinators  (Profunctor, dimap)
import Control.Comonad           (Comonad   , duplicate, extract, extend )

{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b
  rmap  :: (b -> d) -> p a b -> p a d
  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d


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


-- Defining the unique type
-- It seems to be a combination of a Lens and a Prism, such that it is possible the sought after target does not exist, 
-- but at the same time, we need the original context to reassemble new composite types
data AffineP a b s t                         = AffineOp  {   check    ::  s        ->   Either b a, 
       
                                                             recon    ::  (b, s)   ->   t
                                                         }

                        

-- Let's make our Affine into a Profunctor
instance Profunctor (AffineP  a b  ) where
    dimap   h   g   (AffineOp f f' )         =  AffineOp   (f . h) (\x -> g . f' $ (fst x, h . snd $ x)) 
        

---------------------------------------------------------------------------------


--Let's re-use types from the Prism example

data       Crystal           = Crystal


data       Shard             = Shard


-- This is kind-of a composite type; no reason for it to be structured this way, its just for clarity
newtype    Glass   a         = Glass   a


-- This is another kind-of a composite type
newtype    Diamond b         = Diamond b


---------------------------------------------------------------------------------


--Inventing new functions for the types

prep      ::             a'       ->   Glass    a
prep                             = undefined


eject     :: Diamond     b        ->            d
eject                            = undefined


search    :: Glass       a        ->   Either   b  a 
search                           = undefined


raus      :: (b, Glass   a)       ->  Diamond   b
raus                             = undefined

       
 ---------------------------------------------------------------------------------

-- First we make Glass an Applicative, so ....
instance Functor Glass where
    fmap f (Glass x)              =  Glass (f x) 



-- Why this? We need types from a functor context
instance Comonad Glass where
    extract (Glass x)             = x
    duplicate  x                  =  Glass x
    extend     f                  =  fmap f . duplicate



-- We'll need to push these types into an Applicative context
instance Functor Diamond where
    fmap f (Diamond x)            =  Diamond (f x) 


-- So making the diamonds shine... (like a diamond)
instance Applicative Diamond where
  pure x                          =  Diamond x
  Diamond f   <*>  Diamond t      =  Diamond (f t)



 ---------------------------------------------------------------------------------


 -- Let's assemble an actual Profuctor based on our types and computational abilities
affineC :: AffineP Shard Crystal s t
affineC                           =   dimap prep eject . AffineOp search $ raus


-- Now we construct the Affine Optic
-- Seems to be a combination of how we dealt with Lenses and Prisms combined, but now i want to 
-- show that both functions from the provided profunctor can be used to compose between composites
-- Explanation :: 
--      LHS : u goes from (s -> some_choice), but we need to go from composite to composite, so we need to dissociate the chosen type from its context first; or simply create function that can adapt between the twp types
--      RHS : prep acts like h in the definition, so we need to perform something similar to the LHS to define our tuple's RHS;
--            Also, in the attempt to keep the promise of mapping (finally) to composites, we have to reattach the applicative context before releasing it to the covariant function
affineOptic :: AffineP a b s t   -> AffineP a b (Glass s) (Diamond t)
affineOptic    (AffineOp u v)    =   AffineOp (u . extract) (\y  -> eject . pure . v $ (fst y, extract . prep . snd $ y))


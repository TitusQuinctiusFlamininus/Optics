module AffineOpticsFun where

 
import Control.Lens.Combinators  (Profunctor, dimap)

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



--}

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


newtype    Glass   a         = Glass   a


newtype    Diamond b         = Diamond b

---------------------------------------------------------------------------------


--Inventing new functions for the types

prep      ::  a'                   ->   Glass   a
prep      = undefined


eject     :: Diamond b                     ->   d
eject     = undefined


search    :: Glass   a             ->   Either  b  a 
search    = undefined


raus      :: (b, Glass   a)        ->  Diamond b
raus      = undefined

       
 ---------------------------------------------------------------------------------

 -- Let's assemble an actual Profuctor based on our types and computational abilities

appPro :: AffineP a b (Glass   a) (Diamond b )  
appPro =  dimap prep eject (AffineOp search raus)




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
data AffineP a b s t                  = AffineOp  {   peer'  ::  s        ->   Either b a, 
       
                                                      rec    ::  (b, s)   ->   t
                                                  }

                        

-- Let's make our Affine into a Profunctor
instance Profunctor (AffineP a b) where
    dimap h g (AffineOp f f')        =  AffineOp (f . h) (\x -> g . f' $ (fst x, h . snd $ x)) 
        
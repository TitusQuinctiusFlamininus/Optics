module MultistarOpticsFun where

import Control.Lens.Combinators (Profunctor, dimap   )

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
    fmap :: (a -> b) -> f a -> f b


class Functor w => Comonad w where
    extract     ::  w a -> a      
    duplicate   ::  w a -> w (w a)
    extend      :: (w a -> b) -> w a -> w b  

--}   

-- Combination of an Upstar and Downstar
data Multistar   f a b   = Multistar    {  up   :: a     -> f b,
                                           down :: f a   -> b
                                        }
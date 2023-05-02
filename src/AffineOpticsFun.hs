module AffineOpticsFun where

 
import Control.Lens.Combinators  (Profunctor, dimap)
import Control.Comonad           (Comonad, extract, extend, duplicate)

{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b
  rmap  :: (b -> d) -> p a b -> p a d
  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d


--This may come in useful, given the nature of a Downstar
class Functor w => Comonad w where
    extract     ::  w a -> a                       <--- we need only this, technically
    duplicate   ::  w a -> w (w a)
    extend      :: (w a -> b) -> w a -> w b


--}

-- Defining the unique type
data AffineOptical a b s t   = AffineOp  {   peer'  ::  s        ->   Either b a, 

                                             rec    ::  (b, s)   ->   t

                                         }
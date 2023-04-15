module DownstarFun where

import Control.Lens.Combinators (Profunctor, dimap)

{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b
  rmap  :: (b -> d) -> p a b -> p a d
  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d

class Functor w => Comonad w where
    extract :: w a -> a
    duplicate :: w a -> w (w a)
    extend :: (w a -> b) -> w a -> w b

--}

-- Imagining a type that encompasses the idea of a Downstar

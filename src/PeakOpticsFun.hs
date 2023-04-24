module PeakOpticsFun where


 {--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up


-- Combination of an Upstar and Downstar
data Multistar   f a b   = Multistar    {  up   :: a     -> f b,
                                           down :: f a   -> b
                                        }


class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b
  rmap  :: (b -> d) -> p a b -> p a d
  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d

class Functor f where
    fmap :: (a -> b) -> f a -> f b

--}   
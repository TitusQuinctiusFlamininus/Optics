module Cartesian_Strong_OpticsFun where


 {--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b
  rmap  :: (b -> d) -> p a b -> p a d
  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d


where p is a Profunctor : 
type Optic p a b s t = p a b -> p s t


class Profunctor p => Cartesian p where
  first  :: p a b -> p (a, c) (b, c)
  second :: p a b -> p (c, a) (c, b)



--}

   



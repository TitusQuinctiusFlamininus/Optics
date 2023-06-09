module PrismInPhantom where


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


class Profunctor p  =>  Choice p where
  left'    ::  p  a  b   ->  p  (Either  a  c)  (Either  b  c)
  right'   ::  p  a  b   ->  p  (Either  c  a)  (Either  c  b)


class Choice p => InPhantom p where
    icoerce   ::   p  a  c   ->    p  b  


--}   

-- Since we cannot make Prism Choice Profunctors(see PrismCoCartesian), it means we cannot form Prism InPhantom Profunctors

---------------------------------------------------------------------------------
     
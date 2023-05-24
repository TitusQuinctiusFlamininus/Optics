module TaggedCoCartesianFun where


import Control.Lens.Combinators    (Profunctor, dimap)
import Data.Profunctor.Choice      (Choice    , left' , right'  )


 {--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap     :: (c -> a)  ->  p a b     ->  p c b                 <<-----
                                                                      |-------- Either implement these two ...
  rmap     :: (b -> d)  ->  p a b     ->  p a d                 <<-----  

  dimap    :: (c -> a)  ->  (b -> d)  ->  p a b   ->   p c d    <<-------------- Or just this one


class Profunctor p  =>  Cocartesian p where
  left'    ::  p  a  b   ->  p  (Either  a  c)  (Either  b  c)
  
  right'   ::  p  a  b   ->  p  (Either  c  a)  (Either  c  b)


class Functor f where
    <$>     :: (a -> b) -> f a -> f b

--}    


---------------------------------------------------------------------------------

-- Here is the basic type once more....
newtype ChoiceTag  s  b                  =    BranchTag   { untag ::  b }


-- And here is the Profunctor of the Tagged type
instance Profunctor ChoiceTag where
    dimap  _  g  (BranchTag d)           =    BranchTag  (g  d)


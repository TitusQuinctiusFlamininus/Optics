module TraversingLensOpticsFun where 



import Control.Lens.Combinators    (Profunctor, dimap               )
import Data.Profunctor.Traversing  (Traversing, traverse', wander   )
import Data.Profunctor.Strong      (Strong    , first',    second'  )
import Data.Profunctor.Choice      (Choice    , left' ,    right'   )

{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up


class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b                                                         <<-----
                                                                                                     |-------- Either implement these two ...
  rmap  :: (b -> d) -> p a b -> p a d                                                         <<-----  

  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d                                             <<-------------- Or just this one



class Profunctor p => Strong p where
  first'     ::  p  a  b   -> p  (a,  c)  (b,  c)
  second'    ::  p  a  b   -> p  (c,  a)  (c,  b)



where p is a Profunctor : 
type  Optic  p  a  b  s  t    =  p  a  b    ->   p  s  t



---------------------------------------------------------------------------------


class Profunctor p  =>  Cocartesian p where
  left'      ::  p  a  b   ->  p  (Either  a  c)  (Either  b  c)
  right'     ::  p  a  b   ->  p  (Either  c  a)  (Either  c  b)


class (Choice p, Strong p) => Traversing p where
  traverse'  :: Traversable f => p a b -> p (f a) (f b)                                         <<-- Implement Either this ....
  wander     :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a b -> p s t           <<-- Or Implement this


class (Functor t, Foldable t) => Traversable t where
  traverse   :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA  :: Applicative f => t (f a)    -> f (t a)


class Functor f => Applicative f where
    pure      ::    x           -> f x
    <*>       ::    f (a -> b)  -> f a   ->  f b    


class Functor f where
    <$>       ::      (a -> b)  -> f a   ->  f b
--}



---------------------------------------------------------------------------------

-- We cannot create a TraversingLens, based on the Traversing Typeclass. 
-- Why not ? --->>>> We can certainly make a Lens Profunctor Strong.....
--           --->>>> Another requirement of making a Profunctor, a Traversing Profunctor, is that it also needs to be CoCartesian (Choice), which is not possible (see LensCoCartesianOpticalFun)

-- So must abandon the exercise here.                   
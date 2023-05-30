module LensCartesianOpticalFun where 



import Control.Lens.Combinators    (Profunctor, dimap            )
import Data.Profunctor.Strong      (Strong    , first', second'  )

{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up


class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b                 <<-----
                                                             |-------- Either implement these two ...
  rmap  :: (b -> d) -> p a b -> p a d                 <<-----  

  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d     <<-------------- Or just this one


class Profunctor p => Strong p where
  first'  ::  p  a  b   -> p  (a,  c)  (b,  c)
   
  second' ::  p  a  b   -> p  (c,  a)  (c,  b)


where p is a Profunctor : 
type Optic p a b s t = p a b -> p s t


class Functor f where
    <$> :: (a -> b) -> f a -> f b
                

--}



---------------------------------------------------------------------------------

-- Popping up the definition from before
data  StrongLens  a  b  s  t          =    SLens    {   see    ::  s       ->   a,

                                                        update ::  (b, s)  ->   t
                                                    }


-- Making it a Profunctor first like we did before...
instance Profunctor (StrongLens a b) where
    dimap  h  g   (SLens v  w)        =    SLens  (v . h) (\x  ->   g $ w (fst x, (h $ snd x)))


instance Strong (StrongLens a b) where
    first'       (SLens  m   n)       =    SLens   (m . fst) (\x ->  (,)  (n . ((,) (fst x)) $ (fst . snd $ x)) (snd . snd $ x))
    second'      (SLens  m   n)       =    undefined
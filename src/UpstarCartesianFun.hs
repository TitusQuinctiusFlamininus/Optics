module UpstarCartesianFun where

import Data.Tuple                (swap                           )
import Control.Lens.Combinators (Profunctor, dimap               )
import Data.Profunctor.Strong   (Strong    , first', second'     )


 {--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap    :: (c -> a) ->  p a b    -> p c b
  rmap    :: (b -> d) ->  p a b    -> p a d
  dimap   :: (c -> a) ->  (b -> d) -> p a b -> p c d


where p is a Profunctor : 
type Optic p a b s t = p a b -> p s t


class Profunctor p => Cartesian p where
  first  ::  p a b -> p (a, c) (b, c)
  second ::  p a b -> p (c, a) (c, b)


--}


-- Let's revisit our vanilla Upstar and Profunctor Instance
newtype Cartesian f a b         =    StrongStar { upper ::   a  -> f b  }


instance Functor f =>  Profunctor (Cartesian f) where
    dimap h g (StrongStar u)     =   StrongStar (fmap g . u . h) 



-- Alright, now we Strengthen it
instance Functor f =>  Strong     (Cartesian f) where
  first'  (StrongStar  u)           = StrongStar (\(a, x) -> fmap swap . (((,) x) <$>) $ (u a))
  second' (StrongStar  u)           = undefined
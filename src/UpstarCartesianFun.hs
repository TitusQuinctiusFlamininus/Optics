module UpstarCartesianFun where

import Data.Tuple                  (swap                         )
import Control.Lens.Combinators    (Profunctor, dimap            )
import Data.Profunctor.Strong      (Strong    , first', second'  )


 {--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap    :: (c -> a) ->  p a b    -> p c b
  rmap    :: (b -> d) ->  p a b    -> p a d
  dimap   :: (c -> a) ->  (b -> d) -> p a b -> p c d


class Profunctor p => Strong p where
  first'    ::  p a b   -> p (a, c) (b, c)
  second'   ::  p a b   -> p (c, a) (c, b)


class Functor f where
    <$>   :: (a -> b) -> f a -> f b


--}


-- Just for later simplicity
type Tuple' a a'                      =    (a, a')


-- Let's revisit our vanilla Upstar and Profunctor Instance
newtype Cartesian f a b               =    StrongUpStar { upper ::   a  -> f b  }


instance Functor f =>  Profunctor (Cartesian f) where
    dimap h g (StrongUpStar u)        =    StrongUpStar (fmap g . u . h) 



-- Alright, now we Strengthen it
-- Explanation : We simply take the tuple input and feed it to our upstar function, obtaining a functor and subsequently fmap it (if needed) to a partial function depending on tuple type order
instance Functor f =>  Strong (Cartesian f)     where
  first'  (StrongUpStar  u)           =    StrongUpStar (\z -> fmap swap . (((,) . snd $ z) <$>) .  u . fst $ z)
  second' (StrongUpStar  u)           =    StrongUpStar (\z ->             (((,) . fst $ z) <$>) .  u . snd $ z)


---------------------------------------------------------------------------------

-- Revisiting types from Vanilla Upstar, but somewhat simplified

newtype OpFunc a                 = OpFunc a 


preUpstar :: k          ->       a
preUpstar                        = undefined


postUpstar :: b         ->       s'
postUpstar                       = undefined


unstarter :: a          ->       OpFunc b
unstarter                        = undefined


instance Functor OpFunc where
    fmap f (OpFunc x)            = OpFunc (f x)


---------------------------------------------------------------------------------

-- Let's establish a vanilla profunctor from which we can base other functions
vanillaP      :: Cartesian OpFunc a b 
vanillaP               = dimap preUpstar postUpstar . StrongUpStar $ unstarter


-- Let's  create and use strong profunctors from our base type

-- We can now create functors of tuples instead of just types
upStrong   :: Tuple' a a'   ->   OpFunc (Tuple' a a')
upStrong               = upper . first'  $ vanillaP

 
-- Let's keep the tuple argument order the same, but since we are using a different strengthener, we need to swap 
upStrong'  :: Tuple' a a'   ->   OpFunc (Tuple' a' a)
upStrong'              = (upper . second' $ vanillaP) . swap


---------------------------------------------------------------------------------

module UpstarCoCartesianFun where


import Control.Lens.Combinators    (Profunctor, dimap          )
import Data.Profunctor.Choice      (Choice    ,left' , right'  )

 {--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap    :: (c -> a) ->  p a b    -> p c b
  rmap    :: (b -> d) ->  p a b    -> p a d
  dimap   :: (c -> a) ->  (b -> d) -> p a b -> p c d


class Profunctor p => Cocartesian p where
  left'    :: p a b   ->  p (Either a c) (Either b c)
  right'   :: p a b   ->  p (Either c a) (Either c b)


class Functor f where
    <$>   :: (a -> b) -> f a -> f b


--}

-- Let's revisit our vanilla Upstar and Profunctor Instance
newtype CoCartesian f a b               =    ChoiceUpStar { upper ::   a  -> f b  }


instance Functor f =>  Profunctor (CoCartesian f) where
    dimap h g (ChoiceUpStar u)          =    ChoiceUpStar (fmap g . u . h) 


-- Alright, now we Strengthen it
instance Functor f =>  Choice (CoCartesian f)     where
  left'  (ChoiceUpStar  u)           =    undefined
  right' (ChoiceUpStar  u)           =    undefined
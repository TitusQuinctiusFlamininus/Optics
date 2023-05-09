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

-- Let's simply redefine what we had as a basic type, before all the frills
newtype CoCartesian f a b               =    ChoiceUpStar { upper ::   a  -> f b  }


-- Making it a Profunctor is easy enough....
instance Functor f =>  Profunctor (CoCartesian f) where
    dimap h g (ChoiceUpStar u)          =    ChoiceUpStar (fmap g . u . h) 


-- Going ahead and co-strengthening it
-- Note that f needs to be at least an Applicative, not just a Functor
instance (Applicative f) =>  Choice (CoCartesian f)     where
  left'  (ChoiceUpStar  u)              =    ChoiceUpStar . either ((Left <$>) . u   ) $ ((Right <$>) . pure)                                                           
  right' (ChoiceUpStar  u)              =    ChoiceUpStar . either ((Left <$>) . pure) $ ((Right <$>) . u   )               

  -- I'll explain how we got to this arrangement: 
       -- In Upper, when we co-strengthen, instead of (a  -> f b), we are now going like this: ((Either a c)  -> f (Either b c))
       -- Ok, so the input could be Left or Right; So in a CASE statement, we would reason it out this way: 
       --                ----->>>   If it's Left, then we have access to the type a , give it to u, and we get (f b); fmap it with a "Left" and we get: 
       --                                        Left  a     ->  fmap Left (u a) 
       --                ----->>>   If it's Right, then we have access to the type c, lift it into the applicative; fmap it with a "Right" and we get: 
       --                                        Right c     ->  fmap Right (pure u) 
       -- But from our FP basics, we know this function:          either :: (a -> c) -> (b -> c) -> Either a b -> c
       -- The two parts of our case statement represent the first two function of either, and we already have the Either itself, so we just simplify to that form.


---------------------------------------------------------------------------------

-- Let's invent some types we can use as a practical example

-- We can use this as a Functor
newtype Select a                 = Select a 


preUpstar :: a'          ->       a
preUpstar                        = undefined


postUpstar :: b          ->       t
postUpstar                       = undefined


topper     :: a          ->       Select b
topper                           = undefined


-- Functor instance is needed
instance Functor Select where
    fmap f (Select x)            = Select (f x)


-- Remember, it also need to be an Applicative, so let's do that
instance Applicative Select where
  pure   x                       =  Select x
  Select f   <*>  Select v       =  Select (f v)

---------------------------------------------------------------------------------


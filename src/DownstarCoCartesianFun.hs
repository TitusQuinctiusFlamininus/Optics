module DownstarCoCartesianFun where



import Control.Lens.Combinators    (Profunctor, dimap)


 {--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b                 <<-----
                                                             |-------- Either implement these two ...
  rmap  :: (b -> d) -> p a b -> p a d                 <<-----  

  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d     <<-------------- Or just this one




class Profunctor p  =>  Cocartesian p where
  left'    ::  p  a  b   ->  p  (Either  a  c)  (Either  b  c)
  
  right'   ::  p  a  b   ->  p  (Either  c  a)  (Either  c  b)


class Functor f where
    <$>     :: (a -> b) -> f a -> f b

--}


--Checking out the Downstar 
newtype CoStrong  f  a  b                    =     DownCoStar   {  low ::   f  a    ->   b  }


-- Making it a Profunctor is easy enough....
instance Functor f =>  Profunctor (CoStrong f) where
    dimap   h   g    (DownCoStar u)          =     DownCoStar  (  g . u . fmap h )


-- Attempting to Add Choice to our Downstar 
--   It seems like this is not possible

-- Explanation ::  ----->>>>>  The intent is to attempt going from :  (f a    ->   b)     to    :  (f (Either a c)    ->   (Either b c))
--                             Ok. So we need to fmap the input with a function that goes from  :  (  (Either a c)    ->   c           )
--                             If  ( Right c )      :  No problem, we have access to our required type
--                             If  ( Left  a )      :  We could do this now :  ( u . pure  )    :  Which gives us something of type b ......
--                             We still need a function that goes like this :  ( b  ->  c  )    :  We don't have a means to produce such types (c) from types of our output (b)
--                             An additional problem : How do we get rid of the functorial context from fmap ??
--   instance (Functor f) =>  Choice (CoStrong f) where 
--       left'    (DownCoStar u)                  =     Not Possible
--       right'   (DownCoStar u)                  =     Not Possible



---------------------------------------------------------------------------------

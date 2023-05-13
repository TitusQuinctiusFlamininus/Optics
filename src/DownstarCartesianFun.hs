module DownstarCartesianFun where


import Control.Lens.Combinators    (Profunctor, dimap)


 {--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap    :: (c -> a) ->  p a b    -> p c b
  rmap    :: (b -> d) ->  p a b    -> p a d
  dimap   :: (c -> a) ->  (b -> d) -> p a b -> p c d


class Profunctor p => Strong p where
  first'  ::  p a b   -> p (a, c) (b, c)
  second' ::  p a b   -> p (c, a) (c, b)


class Functor f where
    <$>     :: (a -> b) -> f a -> f b

--}


-- Revisiting the type we know by now
newtype CartesianDown   f  a  b      =   StrongDown {  low ::   f  a    ->   b  }


-- And revisiting how we would make this a Profunctor.....
instance Functor f => Profunctor (CartesianDown f) where
  dimap h g ( StrongDown d )         =   StrongDown (  g . d . fmap h )



-- Ok, so far so good. Now let's make it Strong

{--


Why is it not possible to establish a Strong instance for Downstar?
----------->>>>>      First  : Instead of going like this:  (f a  ->  b) , we are going like this:  (f (a, c)   ->   (b, c))
                      Second : Ok, so we have : f (a, c) as input; Let's fmap with a function that takes (a, c)
                      Third  : If we take (a, c) and simply map to a, then the entire fmap computation would produce ; (f a); Ok so far.....
                      Fourth : What can we do with (f a) ? We can apply d to this, giving us : b
-- So it looks like this now :          (\m -> ((d . fmap fst $ m)))
                      Fifth  : We don't need just b, but : (b, c) ; So, if we stretch the same logic to the other part of the tuple, we may get what we need
                      Sixth  : (\m -> ((d . fmap fst $ m),  (fmap snd $ m))) has this type :   (b, f c)  .  
           Now the typechecker complains : "......Couldn't match expected type ‘c’ with actual type ‘f c’......". 

          Here is the best we can do :  ---->>>>>>          StrongDown (\m -> ((d . fmap fst $ m),  (fmap snd $ m)))   :: (b, f c)    <<<<<-------

          Without adjusting/modifying the Contextual type constraint, there is no way to really resolve (f c). 

--*******************
-- DISCLAIMER HERE:
********************* 

   If the definition were changed from : instance Functor f => Strong (CartesianDown f) where 
                                  to   : instance Comonad f => Strong (CartesianDown f) where
   It may be possible to disassociate the second tuple type from its context and continue....    


But, as it stands for now (For Downstar): 
instance Functor f => Strong (CartesianDown f) where
    first'   ( StrongDown d )        =    Not Possible 
    second'  ( StrongDown d )        =    Not Possible 


 --} 

---------------------------------------------------------------------------------

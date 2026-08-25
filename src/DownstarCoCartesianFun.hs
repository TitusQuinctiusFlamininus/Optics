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
--                             Essentially: (\(f (Either a c)) ->  (Left b) or (Right c))   
--                             (1) If (Right c) both sides  : then:  (\(f (Right c)) ->  (Right c)) 
--                                               then: fmap ? (f (Right c)) yields (f (Right c))
--                                               then: fmap (Right c -> Right c) (f (Right c)) yields (f (Right c))
--                                               then: (extract . fmap id) yields (Right c) only if f is also a Comonad
--                             (2) If (Left a) and (Right c) : then:  (\(f (Left a)) ->  (Right c))     
--                                               then: fmap ? (f (Left a)) yields (f (Right c)) 
--                                               then: fmap (Left a -> Right c) (f (Left a)) is problematic, since how to perform (a -> c) is currently unknown        
--                             (3) If (Right c) and (Left b)  : then:  (\(f (Right c)) ->  (Left b))   
--                                               then: fmap ? (f (Right c)) yields (f (Left b))   
--                                               then: fmap (Right c -> Left b) (f (Right c)) is problematic, since how to perform (c -> b) is currently unknown           
--                             (4) If (Left a) and (Left b) : then:  (\(f (Left a)) ->  (Left b))     
--                                               then: fmap ? (f (Left a)) yields (f (Left b)) 
--                                               then: fmap (Left a -> Left b) (f (Left b)) 
--                                               then: fmap (extract . u . pure) fmap (Left a -> Left b) only if f is also a Comonad  
--                             We have at least 2 possibilities where we cannot, either produce some type c or use c to produce a known type                     
--   instance (Functor f) =>  Choice (CoStrong f) where 
--       left'    (DownCoStar u)                  =     Not Possible
--       right'   (DownCoStar u)                  =     Not Possible



---------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module TraversalOpticsFun where



 {--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up


class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b                 <<-----
                                                             |-------- Either implement these two ...
  rmap  :: (b -> d) -> p a b -> p a d                 <<-----  

  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d     <<-------------- Or just this one


class Functor f where
    <$>       :: (a  ->  b)  ->  f a  ->  f b


class Functor f => Applicative f where
    pure      ::  x           -> f x
    <*>       :: f (a -> b)   -> f a   ->  f b      

--}


---------------------------------------------------------------------------------

-- Let's define what we mean by a Traveral 
data Traversal  s  t  a  b      =    forall f. Traversal {  traverse ::  (a -> f b) -> s  -> f t  }


-- Seems like we cannot transform this into a Profunctor: 
-- Explanation :  ------->>>>>>    :  We want to concentrate on something like this       :    Traversal   (__this_thing_in_here__)
--                                 :  We know we are receiving 2 arguments, so let's call them f' and x 
--                                 :  So far, we have                                     :    Traversal  (\f' x  -> something)
--                                 :  Let's get a function that can take things of type s :    (k (f' . h))
--                                 :  (k (f' . h)) is of type                             :    (s -> f t)
--                                 :  We need something of type                           :    (f t)
--                                 :  And x is really of type                             :    s
--                                 :  So, perhaps this is the solution                    :    ((k (f' . h)) x)
--                                 :  Suppose                                             :    d :: (a' -> a)    and         g :: (b -> d)    then: 
--                                 :  ((k (f' . h)) x) is problematic because             :    f' here is now :  (a' -> f d) instead of   : (a' -> f b)
--                                 :  This cannot be resolved. 

--instance Profunctor   (Traversal  s  t ) where
--      dimap    h    g   (Traversal  k)     =   ??
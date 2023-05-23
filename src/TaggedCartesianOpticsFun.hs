module TaggedCartesianOpticsFun where


import Control.Lens.Combinators    (Profunctor, dimap)


 {--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap    :: (c -> a) -> p a b -> p c b                 <<-----
                                                             |-------- Either implement these two ...
  rmap    :: (b -> d) -> p a b -> p a d                 <<-----  

  dimap   :: (c -> a) -> (b -> d) -> p a b -> p c d     <<-------------- Or just this one


class Profunctor p => Strong p where
  first'  ::  p  a  b   -> p  (a,  c)  (b,  c)
   
  second' ::  p  a  b   -> p  (c,  a)  (c,  b)


class Functor f where
    <$>     :: (a -> b) -> f a -> f b

--}    


---------------------------------------------------------------------------------


-- Remembering the type...
newtype StrongTag  s  b                  =    CartTag   { untag ::  b }


-- Making it a Profunctor is not hard....
instance Profunctor StrongTag where
    dimap  _  g  (CartTag m)             =    CartTag  (g  m)


--  Now let's make it Strong...
-- Seems we cannot : Why ?

-- ------->>> Explanation: 
--                    According to the Strong requirements, we need to end up with :  CartTag (some_tuple)
--                    The tuple needed is simply : (b, c), where : b is really v (see below)
--                    So far then, we have       : (v, c)  ......
--                    But what is type c ?       : We have no way of knowing what this type is, how to obtain it, or even how to produce it from other types (or functions). 
--                                                 Our hands are tied.  

-- instance Strong StrongTag where
--      first'   (CartTag v)               =    ?? (seems impossible)
--      second'  (CartTag v)               =    ?? (seems impossible)


---------------------------------------------------------------------------------


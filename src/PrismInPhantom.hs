module PrismInPhantom where


import Control.Lens.Combinators    (Profunctor, dimap           )

---------------------------------------------------------------------------------

 {--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up


class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b                 <<-----
                                                             |-------- Either implement these two ...
  rmap  :: (b -> d) -> p a b -> p a d                 <<-----  

  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d     <<-------------- Or just this one



where p is a Profunctor : 
type Optic p a b s t = p a b -> p s t


class Profunctor p  =>  Choice p where
  left'    ::  p  a  b   ->  p  (Either  a  c)  (Either  b  c)
  right'   ::  p  a  b   ->  p  (Either  c  a)  (Either  c  b)


class Choice p => InPhantom p where
    icoerce   ::   p  a  c   ->    p  b  c

--}   


---------------------------------------------------------------------------------
   
-- Revisiting Prism
data Prism a b s t     = Prism {    exist    :: s     ->     Either b a, 

                                    recon    :: b     ->     t
                               }



-- Creating the Prism Profunctor as before....
instance Profunctor (Prism s t) where 
     dimap  h  g  (Prism  u  v)   = Prism (u . h)  (g . v)       


-- First, since InPhantoms are Choices, we need to make the Prism a Choice
-- It seems as though we cannot make Prisms into Choices
-- Explanation :   ---------->>>     For exist function    :  The transition is from : (s   ->   Either b a)  into : ((Either s c)   ->   (Either b a))
--                                   If        (Left  s)   :  Then k represents exactly what we need, since we have access to s, in the either function
--                                   If        (Right c)   :  We have access to type c, but there is a problem : (c -> Either b a)
--                                   How can we create (Either b a) from things of type c ? We do not know how to. There is no (known) way to.
--                 ---------->>>     For recon function    :  The transition is from : (b   ->   t)    into  :     (b   ->   Either t c)
--                                   If        (Left t )   :  The  (Left . w) is the only way to adequately construct (Either t c)
--                                   If        (Right c)   :  We already have a means of creating (Either t c), from (Left . w) above, since c will be on the Right anyway....  
--             :    So the only problem was in the exist function. For right' the problem does not disappear, simply because type c has shifted position in Either                              
--instance Choice     (Prism s t) where
 --   left'         (Prism  k  w)   = Prism ( either k ?? ) (Left . w)
--    right'        (Prism  k  w)   = ??


---------------------------------------------------------------------------------
     
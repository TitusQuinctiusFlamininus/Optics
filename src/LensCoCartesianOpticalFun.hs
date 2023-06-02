module LensCoCartesianOpticalFun where 



import Control.Lens.Combinators    (Profunctor, dimap            )


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


where p is a Profunctor : 
type  Optic  p  a  b  s  t    =  p  a  b    ->   p  s  t


--}



---------------------------------------------------------------------------------

-- Exposing the Vanilla definition
data  ChoiceLens  a  b  s  t          =    Lens    {    seek    ::  s       ->   a,

                                                        fix     ::  (b, s)  ->   t
                                                    }



-- Again, our Vanilla Profunctor....
instance Profunctor (ChoiceLens a b) where
    dimap  h  g   (Lens q  r )         =    Lens   ( q . h ) (\y  ->  g $ r (fst y, (h $ snd y)))


-- Attempting to make the Lens CoCartesian .... Not Possible! How come?
--    -------->>>>    For seek                         :
--                 For Left'  :  We are shifting from  :   (s  ->   a)   to       :  ((Either s c)  ->  a) 
--                            :  ----->>     If   (Left  s)   : then it is possible to create the required type
--                            :              If   (Right c)   : we have no way of creating the required type (c -> a)
--    -------->>>>    For fix                          : 
--                 For Left'  :  We are shifting from  :   ((b, s)  ->  t)   to   :  ((b, (Either s c))  ->  (Either t c)) 
--                            :  ----->>     If   (Left s)    : It may be possible to form the fix function's tuple  :  ((fst y), w)  (where y is (b,s) and w is s within the first argument of "either")
--                            :                   Two problems now exist : First, we have no way to create t's, because fix needs a Lens of type : (ChoiceLens a b s (Either t c)), but what we provide on the L.H.S is only (ChoiceLens a b s t)
--                            :                                          : Second, within the first argument function of an "either", we have no access to type c , (Right c), to create  this tuple: (t, c)

--   -------->>>   Right' will not change things, since all that really happens is a Contextual positional shift between (c and s), and between (c and t)
--instance Choice (ChoiceLens  a  b) where
--    left'          (Lens v  w )         =    ??    (Cannot be formed)
--    right'         (Lens v  w )         =    ??    (Cannot be formed)
                                                         
module IsoCoCartesianOpticsFun where

import Control.Lens.Combinators    (Profunctor, dimap           )
import Data.Profunctor.Choice      (Choice    , left' , right'  )

 {--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap     :: (c -> a)  ->  p a b     ->  p c b                 <<-----
                                                                      |-------- Either implement these two ...
  rmap     :: (b -> d)  ->  p a b     ->  p a d                 <<-----  

  dimap    :: (c -> a)  ->  (b -> d)  ->  p a b   ->   p c d    <<-------------- Or just this one


-- Also known as Choice
class Profunctor p  =>  Cocartesian p where
  left'    ::  p  a  b   ->  p  (Either  a  c)  (Either  b  c)
  
  right'   ::  p  a  b   ->  p  (Either  c  a)  (Either  c  b)


--} 


-- Rehashing the Isomorphism Computational Structure (from IsoCartesianOpticsFun)
data  Iso  a  b  s  t                   =     Iso    {   hin    ::  a       ->   b,
 
                                                         her    ::  s       ->   t
                                                    
                                                     }


-- Quickly making the Iso a Profunctor
instance  Profunctor (Iso a  b  )   where
    dimap    h    g  (Iso f  k  )       =    Iso   f   (g . k . h)


-- Alright, now we (attempt) to make it Choice, or CoCartesian 
-- We are able to make the co-cartesian case successfully in the following manner: 
-- --->>>>>>>>>>>>>>>>>> EXPLANTION
-- ---->>>              :  For Left'  :: The hin function remains untouched, because no type variables appear for us to manipulate (s and t types)
--                                    :: The her function: 
--                                      -->> The transformation left' demands is from : (s  -> t)  into      :  ((Either s c)  -> (Either t c))
--                                           Our input is now either a Left or Right structure, specifically :  (Left s)  or (Right c)       
--                                           (Left s) : we have a function that has s as input, but it must output (Either t c)  : (\s ->  Either t c)
--                                                    : Since the output must be "left-leaning" for s (for left'), the output must be restricted to   :  (Left t)
--                                                    : So what we really have is a function:  (\s -> Left t) , or :  (\s -> Left . k $ s), or : (Left . k) 
--                                           (Right c): then we have a function that has c as input, but it must output (Either t c)   : (\c ->  Either t c)
--                                                    : Since the output must be "right-leaning)" for c (for left'), the output must be restricted to : (Right c)     
--                                                    : So what we really have is a function:  (\c -> Right c), or :  Right    
-- ----->>>             : For Right'  :: The hin function still remains untouched, because no type variables appear for us to manipulate (s and t types)
--                                    :: The her function: 
--                                      -->> The transformation right' demands is from : (s  -> t)  into      :  ((Either c s)  -> (Either c t))
--                                           Our input is now either a Left or Right structure, specifically :  (Left c)  or (Right s)       
--                                           (Left c) : we have a function that has c as input, but it must output (Either c t)  : (\c ->  Either c t)
--                                                    : Since the output must be "left-leaning" for c (for right'), the output must be restricted to   :  (Left c)
--                                                    : So what we really have is a function:  (\c -> Left c) , or :  Left
--                                           (Right s): we have a function that has s as input, but it must output (Either c t)   : (\s ->  Either c t)
--                                                    : Since the output must be "right-leaning)" for s (for right'), the output must be restricted to : (Right t)     
--                                                    : So what we really have is a function:  (\s -> Right t), or : (\s -> Right (k s)) , or : (Right . k)  
instance Choice (Iso a b) where
  left'     (Iso   f   k)            =   Iso   f   (either (Left . k) Right      )
  right'    (Iso   f   k)            =   Iso   f   (either  Left     (Right . k) )

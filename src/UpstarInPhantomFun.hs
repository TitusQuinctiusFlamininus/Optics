module UpstarInPhantomFun where


import Control.Lens.Combinators    (Profunctor, dimap           )
import Data.Profunctor.Choice      (Choice    , left' , right'  )


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
    <$>         :: (a -> b) -> f a -> f b


--}


---------------------------------------------------------------------------------


class Choice p => InPhantom p where
    icoerce   ::   p  a  c   ->    p  b  c



-- Let's simply redefine what we had as a basic type, before all the frills
newtype Upstar f  a  b               =    UpPhantom { upper ::   a  -> f b  }



-- Making it a Profunctor is easy enough....
instance Functor f =>  Profunctor (Upstar f) where
    dimap h g (UpPhantom u)          =    UpPhantom (fmap g . u . h) 



-- Also, making it Choice ....
instance (Applicative f) =>  Choice (Upstar f)     where
  left'  (UpPhantom  u)              =    UpPhantom . either ((Left <$>) . u   ) $ ((Right <$>) . pure)                                                           
  right' (UpPhantom  u)              =    UpPhantom . either ((Left <$>) . pure) $ ((Right <$>) . u   )               


-- Seems like an instance cannot be formed 
---  ----------->>>>>> Explanation :  The transformation is from   :   (a   ->  f c)   to  :  (b  ->  f c)
--                                 :  There's access to type b, but we need to form (f c)
--                                 :  To form (f c), we may use u  :   But we need access to type a  
--                                 :  So really, we need a function that can transform b's into a's    :   (b  ->  a)  ..... ??? This function is unknown
--                                 :  Side Note : If you REALLY force the issue, we could use unsafe package's:  coerce :: (b -> a) (for practical cases, but not in principle)
--instance (Applicative f) =>  InPhantom (Upstar f) where
--    icoerce  (UpPhantom  u)        =    UpPhantom $ u . ??



---------------------------------------------------------------------------------

module ForgetFun where

import Control.Lens.Combinators  (Profunctor, dimap)

{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b
  rmap  :: (b -> d) -> p a b -> p a d
  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d



--}


---------------------------------------------------------------------------------

-- Inventing our own Forgetful type 
-- We completely forget about one of our types, effectively making it a phantom

data    ForgetFul  r  a  b         =      Forget {   forget ::  a   ->  r  }


-- Alright, let's make it a Profunctor 

-- Apparently we don't care what covariant function is provided : 
--   ------>>> Why ? From the definition above  : we need to map from (c -> d), and we can provide type a through h; 
--                   Normally, our ForgetFul type is meant to give us a type we can provide to our covariant function, but we have "forgotten" that type; it will never be provided
--                   This means that it does not matter that the covariant function does;
--                   all we know is that whatever type the whole profunctor finally points to will be the same type as the last computational type we last produced : So d must be identical to r
--                   We can re-write the definition, in this case, to be :  {  dimap :: (c -> a) -> (r -> r) -> p a r -> p c r  }  =  {  dimap :: (c -> a) -> id -> p a r -> p c r  }    

instance Profunctor (ForgetFul r) where
    dimap  h  _  (Forget f )       =      Forget (f . h)
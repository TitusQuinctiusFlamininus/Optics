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

data    ForgetFul  r  a  b             =       Forget {   forget ::  a   ->  r  }


-- Alright, let's make it a Profunctor 

-- Apparently we don't care what covariant function is provided : Why ? 
--   ------>>>       From the definition above  : we need to map from (c -> d), and we can provide things of type a through h; 
--                   Normally, our ForgetFul type is meant to give us a type we can provide to our covariant function, but we have "forgotten" that type; it will never be provided
--                   This means that it does not matter what the covariant function does, or even what types it maps; it could map things of type r, who knows...
--                   All we know is that whatever type the entire computation points to must be the same type as the last computational type we produced (that we COULD produce) : So d must be identical to r
--                   We can re-write the definition, in this case, to be :  If (r' == r), {  dimap :: (c -> a) -> (r' -> r) -> p a r -> p c r  }  =  {  dimap :: (c -> a) -> id -> p a r -> p c r  } 
--                                                                       :  If (r' /= r), it still will not matter, because it is not certain if we could provide r' from Forget 
instance Profunctor (ForgetFul r) where
       dimap  h  _  ( Forget f  )       =      Forget (f . h)



---------------------------------------------------------------------------------

-- Let's re-use types from the Lens module

data    Atom                    =   Atom


---------------------------------------------------------------------------------

-- Also re-visiting 2 functions defined in the Lens module

-- This contravariant function will supply our original structure
before         ::   m         ->   Atom
before                          = undefined


forgetting     ::   Atom      ->   w
forgetting                      = undefined


---------------------------------------------------------------------------------

-- We are in a position to roll our own concrete Profunctor, using the identity function as a lame duck substitute
-- Even if we promised to return the same type as forgetting does, from the b-type signature, it has no effect
forgetP :: ForgetFul  w  Atom  w
forgetP                         = dimap before id  (Forget forgetting)


---------------------------------------------------------------------------------

module TaggedOpticsFun where


import Control.Lens.Combinators    (Profunctor, dimap)


 {--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b                 <<-----
                                                             |-------- Either implement these two ...
  rmap  :: (b -> d) -> p a b -> p a d                 <<-----  

  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d     <<-------------- Or just this one



class Functor f where
    <$>     :: (a -> b) -> f a -> f b

--}    


---------------------------------------------------------------------------------


-- Defining the type
-- One of our types, s,  seems to be a phantom. 
newtype Tagged  s  b                =    FunTag   { untag ::  b }



-- Making the Tagged type a Profunctor
-- The contravariant function would have no effect on the Tagged type; 
-- This means that the only requirement left is to make sure FunTag points to the same resultant type as the covariant function : d 
-- To do that, we simply supply the that function with what it needs to give us that type : b
instance Profunctor Tagged where
    dimap  _  g  (FunTag n)         =    FunTag  (g  n)



---------------------------------------------------------------------------------


-- Let's come up with a simple type example


-- We cannot really see this type 
data Ghost      =   Ghost



-- When something is tagged, it seems to be of this type
data It         =   It 


-- Let's invent a real type
data Actual       =   Actual


-- Now we can invent a function that polishes a Tag
stamp         ::   r    ->     u 
stamp           = undefined



-- This one polishes a Tag in a down-to-earth way
stamp'         ::   r    ->     Actual
stamp'           = undefined


---------------------------------------------------------------------------------


-- So then we define the Profunctor in terms of the invented types
tagP          ::  Tagged  Ghost  It
tagP             = dimap  id    stamp   .  FunTag $ It




-- Here is another completely ghoulish Profunctor, re-using the covariant function as a contravariant one
tagP'         ::  Tagged  ()  Ghost
tagP'            = dimap  stamp  stamp  .  FunTag $ Ghost



-- We can even create new (actual) Tagged Profunctor types, by combining profunctor factories...
youAreIt      :: Tagged  x Actual
youAreIt         =  dimap  (stamp . untag $ tagP) stamp'  . FunTag . FunTag . untag $ tagP'
  


---------------------------------------------------------------------------------


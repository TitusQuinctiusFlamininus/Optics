module TaggedCoCartesianFun where


import Control.Lens.Combinators    (Profunctor, dimap)
import Data.Profunctor.Choice      (Choice    , left' , right'  )


 {--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap     :: (c -> a)  ->  p a b     ->  p c b                 <<-----
                                                                      |-------- Either implement these two ...
  rmap     :: (b -> d)  ->  p a b     ->  p a d                 <<-----  

  dimap    :: (c -> a)  ->  (b -> d)  ->  p a b   ->   p c d    <<-------------- Or just this one


class Profunctor p  =>  Cocartesian p where
  left'    ::  p  a  b   ->  p  (Either  a  c)  (Either  b  c)
  
  right'   ::  p  a  b   ->  p  (Either  c  a)  (Either  c  b)


--}    


---------------------------------------------------------------------------------

-- Here is the basic type once more....
newtype ChoiceTag  s  b                  =    BranchTag   { untag ::  b }



-- And here is the Profunctor of the Tagged type
instance Profunctor ChoiceTag where
    dimap  _  g  (BranchTag d)           =    BranchTag  (g  d)


-- We can make it a Cartesian quite easily
-- A question arises: ----->>>>>    Why seal the choice to Left for left' and Right for right' respectively ? After all, "Either" could be Left OR Right....
--                                  The reason is that : For left' :  If the choice were RIGHT: We'd be dealing with things of type c (see definition above);
--                                                                 :  Now, we know we can't form things of type c from other types or functions, nor do we have access to it in any other way....
--                                                                 :  Also, ChoiceTag deals with only a single type in its constructor; 
--                                                                 :  So either we cannot form a left' instance, or we go with the lowest common denominator and choose a side! We HAVE to use the LEFT choice
--                                  Same logic        : For right' :  This time, the only type we have access to is on the RIGHT; so we bind it to the RIGHT constructor
instance Choice ChoiceTag where
              left'                      =    BranchTag . Left  . untag 
              right'                     =    BranchTag . Right . untag 



-- The examples used in the Original Tag example (TaggedOpticsFun) can serve as a similar concrete example. The constructor 
-- in our case, will simply be associated with a Either constructor instance, but the functions are identical

---------------------------------------------------------------------------------



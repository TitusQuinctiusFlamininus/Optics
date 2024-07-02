module PrismCartesianFun where



import Data.Either.Utils           (fromRight          )
import Control.Lens.Combinators    ( Profunctor, dimap )

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
type   Optic  p  a  b  s  t   =   p  a  b   ->  p  s  t


class Profunctor p => Strong p where
  first'  ::  p  a  b   -> p  (a,  c)  (b,  c)
   
  second' ::  p  a  b   -> p  (c,  a)  (c,  b)


--}   


---------------------------------------------------------------------------------
   
--Formally defining an Optic 
type Optical a   b   s   t   =   (Prism  a  b  a  b   ->  Prism  a  b  s  t)


-- Defining a Strong Profunctor
class Profunctor p => Strong p where
  first'  ::  p  a  b   -> p  (a,  c)  (b,  c)
   
  second' ::  p  a  b   -> p  (c,  a)  (c,  b)


-- Our Prism Definition
data Prism a  b  s  t  = SPrism {   seek  ::  s   ->   Either b  a, 

                                    fill  ::  b   ->   t
                                }


-- Make the Prism a Profuntor
-- Creating the Prism Profunctor as before....
-- If how this Profunctor was formed is not clear, please see the PrismOpticsFun module for a full explanation
-- It is located here : https://github.com/TitusQuinctiusFlamininus/Optics/blob/main/src/PrismOpticsFun.hs
-- I have used different type symbols here but it is essentially the same (just needs some eye squinting)
instance Profunctor (Prism  s  t) where 
    dimap   d   g   (SPrism k  m)   =   SPrism  (k . d)   (g . m)

-- And now attempting to Strengthen the Prism...

-- Explanation FOR FIRST': 
-- -----------------> For the LEFT-HAND-SIDE :
--                 --  We require a function like this:  (\(s, c)  ->  Either b  a) where c is some type
--                 --  Let's take advantage of the first tuple element for the solution: (\x -> ?? .  fst $ x)
--                 --  Provide the result to k:  (\x -> k . fst $ x), resulting in point-free notation to:  (k . fst)
-- -----------------> For the RIGHT-HAND-SIDE :
--                 --  We require a function like this:  (\b   ->  (t, c)) where c is some type
--                 --  Let's provide that function's input to m:  (\b  -> m $ b), which resolved into: t
--                 --  But: We need (t, c) and not just t..... So let's wrap everything in a tuple with the some type c..
--                 --  Type c is unknown, so let's leave it undefined:  (\b  -> ((m $ b), ??))  becomes:   (\b  -> ((m $ b), undefined))
-- Explanation FOR SECOND': 
-- -----------------> For the LEFT-HAND-SIDE :
--                 --  Only difference with first' is the position of the unknown type c. Therefore, the snd tuple function is needed
-- -----------------> For the RIGHT-HAND-SIDE :
--                 -- Again, just positioning: function application occurs in the second tuple position


instance Strong (Prism  s  t) where 
      first'    (SPrism k  m)       =    SPrism (k . fst) (\x  -> (    (m x),   undefined))
      second'   (SPrism k  m)       =    SPrism (k . snd) (\x  -> (undefined,       (m x)))


---------------------------------------------------------------------------------
-- These are also some types we had before, but redefining here for convenience 

-- we need a function that will provide the materials to make a wonderful prism
preheat      :: a'           ->  s
preheat                       =  undefined

-- Now we need something that will polish up our prism before we display it
cool         :: t            ->  d
cool                          =  undefined

magnify      :: s            ->  Either b  a
magnify                       =  undefined

-- This function can build a new structure from fragments of new material, using the prism
pressurize   :: b            ->  t
pressurize                    =  undefined


---------------------------------------------------------------------------------
-- Forming Profunctors:

-- Provide a function whose output is useful to the covariant function, and it will provide a full prism
basePrism      ::  (b  ->  t)  -> Prism  a  b  s  t
basePrism      =   dimap preheat cool . SPrism magnify


-- Let's strengthen the base prism one way to form a profunctor...
xPrismF        :: Prism  a  b  (s, c)  (t, c)
xPrismF        =  first' . basePrism $ pressurize


-- And in the other way to form another profunctor..
yPrismS        :: Prism  a  b  (c, s)  (c, t)
yPrismS        =  second' . basePrism $ pressurize


---------------------------------------------------------------------------------
-- Finally the Cartesian Prism Optics:

-- This Optic can produce a Strengthened Profunctor of one Kind from a simpler one that computes "internal" types...
xPrismOpticF   :: Prism  a  b  a  b  ->  Prism  a  b  (s, c)  (t, c)
xPrismOpticF        (SPrism x _)   =    SPrism (x . rip snd                           ) 
                                               (\z -> (pressurize . id $ z, undefined))


-- If you already have a strengthened profunctor, this Optic will create a strong computational one to transpose between composites
xStrongPrismOpticF   :: Prism  a  b  (a, c)  (b, c)  ->  Prism  a  b  (s, c)  (t, c)
xStrongPrismOpticF  (SPrism x y)   =    SPrism (\z   ->  x (rip snd z, snd z                      )) 
                                               (\z   ->    (pressurize . fst . y $ z, snd . y $ z ))


-- First, the Optic for the second form of strength, still dealing with "internal" types...
yPrismOpticF   :: Prism  a  b  a  b  ->  Prism  a  b  (c, s)  (c, t)
yPrismOpticF        (SPrism x _)   =    SPrism (x . rip snd                           )  
                                               (\z -> (pressurize . id $ z, undefined))


-- Now dealing with the already strengthened alternative form...
yStrongPrismOpticF   :: Prism  a  b  (c, a)  (c, b)  ->  Prism  a  b  (c, s)  (c, t)
yStrongPrismOpticF  (SPrism x y)   =    SPrism (\z -> x (fst z, rip snd z                      )) 
                                               (\z   -> (fst . y $ z, pressurize . snd . y $ z ))


-- A Curious yet Fun Optical Structure: It transforms a Strong Prism from one "direction" to Strong Prism in another 
-- One can also go the other way around, both from simpler elemental types to composites, or purely elementals....
zStrongPrismOpticF   :: Prism  a  b  (a, c)  (b, c)  ->  Prism  a  b  (c, s)  (c, t)
zStrongPrismOpticF  (SPrism x y)   =    SPrism (\z   ->  x (rip snd z, fst z                      )) 
                                               (\z   ->    (snd . y $ z, pressurize . snd . y $ z ))


---------------------------------------------------------------------------------

-- We can form a xPrismF profunctor from a lower profunctorial form like this...
xDirect ::  Prism  a  b  (a, c)  (b, c)  ->  Prism  a  b  (s, c)  (t, c)
xDirect k    =     SPrism (\v -> seek k (rip snd v, rip snd v                     ))   
                          (\v -> (pressurize . fst . fill k $ v, snd . fill k $ v ))


-- Similarly, same for a yPrismF profunctor from another compositional type...
yDirect ::  Prism  a  b  (c, a)  (c, b)  ->  Prism  a  b  (c, s)  (c, t)
yDirect k    =     SPrism (\v -> seek k (rip fst v, rip fst v                      ))
                          (\v ->  (fst . fill k $ v, pressurize . snd . fill k $ v ))


-- Convenience Function 
rip :: ((k, m) -> r) -> (k, m)  -> n
rip          = fromRight . magnify


---------------------------------------------------------------------------------
module PrismCartesianFun where



import Data.Either.Utils
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
   
--F ormally defining an Optic 
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
--                 -- We have no information about what c is exactly, or how to produce it. It could be any type! If only we had: (b -> c) or (t -> c)
--                 --  So let's leave it undefined:  (\b  -> ((m $ b), ??))  becomes:   (\b  -> ((m $ b), undefined))
-- Explanation FOR SECOND': 
-- -----------------> For the LEFT-HAND-SIDE :
--                 --  Only difference with first' is the position of the unknown type c. Therefore, the snd tuple function is needed
-- -----------------> For the RIGHT-HAND-SIDE :
--                 -- Again, just positioning: function application occurs in the second tuple position

instance Strong (Prism  s  t) where 
      first'    (SPrism k  m)       =    SPrism (k . fst) (\x  -> (    (m x),   undefined))
      second'   (SPrism k  m)       =    SPrism (k . snd) (\x  -> (undefined,       (m x)))


---------------------------------------------------------------------------------

-- Let's reuse some types we defined way back in the vanilla prism 

data       Crystal           = Crystal

data       Shard             = Shard

-- I'm imagining these to be analogous to composite types

newtype    Glass   a         = Glass   a

newtype    Diamond b         = Diamond b

---------------------------------------------------------------------------------
-- These are also some types we had before, but redefining here for convenience 
-- Keeping in mind: (P a b s t) 

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

-- If we provide this function a function our covariant function can use, it gives us back a prism
basePrism      ::  (b  ->  t)  -> Prism  a  b  s  t
basePrism      =   dimap preheat cool . SPrism magnify


-- Let's make some Strengthened Profunctor with the above definitions strong, in the first way
xPrismF        :: Prism  a  b  (s, c)  (t, c)
xPrismF        =  first' . basePrism $ pressurize


-- Now we strengthen the other way
yPrismS        :: Prism  a  b  (c, s)  (c, t)
yPrismS        =  second' . basePrism $ pressurize

---------------------------------------------------------------------------------
-- Settings up some Optics 

-- EXPLORE : xPrismOpticF   :: Prism  a  b  a  b  ->  Prism  a  b  (s, c)  (t, c)


-- Ok, now we can form a Strong Optic in the first way
xPrismOpticF   :: Prism  a  b  (a, c)  (b, c)  ->  Prism  a  b  (s, c)  (t, c)
xPrismOpticF  (SPrism x y)   =    SPrism (\z   ->  x ((fromRight . magnify . fst $ z), snd z  )) 
                                         (\z'  ->    (pressurize . fst . y $ z', snd . y $ z' ))
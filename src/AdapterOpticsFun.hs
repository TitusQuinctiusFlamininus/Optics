module AdapterOpticsFun where

import Control.Lens.Combinators (Profunctor, dimap)

{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b
  rmap  :: (b -> d) -> p a b -> p a d
  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d

where p is a Profunctor : 
type Optic p a b s t = p a b -> p s t

--}

--This will be an attempt to take the concept of an Adapter, and make an initial Profunctor, subsequent Optics and 
-- a final profunctor that will traverse between input whole types (s) into target types (t)

--Let's define what we need, a type that we can work with
-- We presume      : things of type a can be found within structures of type s
-- We also presme  : we can create new structures of type t, simply by replacing things of type a with things of type b, within structures of type s
data FunAdapter a b s t = FAdapter { to  :: s -> a , 
                                     fro :: b -> t 
                                   }

-- Let's make it a Profunctor
instance Profunctor (FunAdapter a b) where
    dimap h g (FAdapter i o) = FAdapter (i . h) (g . o)


 ---------------------------------------------------------------------------------

-- Lets invent more types to use in our example


-- we will be going from structures of type s, that could be defined like this (as an example)...
newtype Raw a              = Raw a

-- Its possible to end up with structures like this of type t, that could be defined like this (as an example)...
newtype Ripe b             = Ripe b 

-- We can transpose to things of this type from s
data Old                   = Old 

-- We can compose things of type t with this type
data New                   = New
---------------------------------------------------------------------------------                       


-- So now lets invent some functions that can take advantage of our types

-- Lets invent a contravariant function that provides the dimap something of type s
preAdapt :: s' -> Raw Old
preAdapt  = undefined

-- We also need a covariant function that takes our profunctor output (t) and potentially manipulates it further 
postAdapt ::Ripe New -> t'
postAdapt = undefined

-- Now we invent a function that represents how our adapter works primarily
adapt :: Raw Old -> Old
adapt  = undefined

-- Now we invent a function that represents how our adapter works primarily
unAdapt :: New -> Ripe New
unAdapt  = undefined


---------------------------------------------------------------------------------
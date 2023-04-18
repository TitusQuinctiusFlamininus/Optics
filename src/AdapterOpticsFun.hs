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
postAdapt :: Ripe New -> t'
postAdapt = undefined

-- Now we invent a function that represents how our adapter works primarily
adapt :: Raw Old -> Old
adapt  = undefined

-- Now we invent a function that represents how our adapter works primarily
unAdapt :: New -> Ripe New
unAdapt  = undefined


---------------------------------------------------------------------------------

-- Let's define our transformation with what we have so far: 
--This Profunctor goes from types : s' -> t' (choosing (Raw Old) and (Ripe New) as s' and t')
--adapterP :: FunAdapter Old New s' t'
adapterP :: FunAdapter Old New (Raw Old) (Ripe New)
adapterP = dimap preAdapt postAdapt (FAdapter adapt unAdapt)

-- We can write what we have above kinda like this:
--adapterP  :: FunAdapter a b s' t'        <<------- (which was going from an s' to t')

-- That's fine, but what if we wanted this:
--adapterP' :: FunAdapter a b s t          <<------- (which was going from an s to t) 

-- In order words, we need a function that will have this type: 
-- myUnknownOptic :: p a  b ->  p s t
-- Or more accurately:  p s' t' -> p s t

-- Remember waaaaaaaay at the top of the page, we wrote this wierd thing:         ---->>>>     Optic p a b s t = p a b -> p s t
-- Looks kinda like what we want to achieve in the "myUnknownOptic", right? 


-- What would our final Signature looks like, if we filled in the abstract types (a, b, s, t) with types we already invented?
 --adapterOptical :: FunAdapter Old New s' t' -> FunAdapter Old New (Raw Old) (Ripe New)
 -- Now since s' can be supposed to be (Raw Old) and t' can be supposed to be (Ripe New) for all intents and purposes, we can simply the signature

 -- So lets invent it now: It becomes:
 -- Optic p Old New (Raw Old) (Ripe New) =  (p Old New   -> p (Raw Old) (Ripe New))
adapterOptical :: FunAdapter Old New (Raw Old) (Ripe New) -> FunAdapter Old New (Raw Old) (Ripe New)
adapterOptical (FAdapter i o) = dimap i o (FAdapter id id)

-- Explanation : dimap's inside i  function has a signature  (s -> a), and our "to"  adapter function needs to produce the same type as well (to  :: s -> a) : so why not use the identity function to simply carry the "a" along unmodified, instead of creating an s and then grinding it back to to an "a" again
-- Also        : dimap's outside o function has a signature  (b -> t), and our "fro" adapter function needs to produce the same type as well (fro :: b -> t) : so why not use the identity function to simply carry the "t" along unmodified, essentially assuming our "b"s to be "t"s

--But how can we actually take advantage of the adapterP profunction we made earlier? Just hand it over to our Optical
-- Now we have a Profunctor that can map between whole structures, instead of just the types found within those structures 
useOptical :: FunAdapter Old New (Raw Old) (Ripe New)
useOptical = adapterOptical adapterP

      
-- Creating some adapter utility optic for To, that can give a's from s's
useOpticalTo :: Raw Old -> Old
useOpticalTo = to useOptical

-- Creating some adapter utility function for Fro, that will compose t's from b's   
useOpticalFro :: New  -> Ripe New
useOpticalFro = fro useOptical
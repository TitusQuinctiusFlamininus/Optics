{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}


module MultistarStrongSieveOpticsFun where


import Control.Comonad             (Comonad   , extract   , duplicate, extend   ) 
import Control.Lens.Combinators    (Profunctor, dimap                           )
import Data.Profunctor.Strong      (Strong    , first'    , second'             )

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
type Optic  p a b s t    =    p a b     ->    p s t



class (Profunctor p, Functor f) => Sieve p f | p -> f where
        sieve    ::    p a b    ->    a   ->    f b
  


-- We will have to change this contextual signature to make the experimental type Strong successfully
-- If not, we cannot continue, so a little bit of rule-bending is needed
class Profunctor p => Strong p where
        first'   ::  p  a  b   -> p  (a,  c)  (b,  c)
        second'  ::  p  a  b   -> p  (c,  a)  (c,  b)



-- As a convenience only so we can develop our example further than it normally is possible
class Functor w => Comonad w where
        extract   :: w a -> a
        duplicate :: w a -> w (w a)
        extend    :: (w a -> b) -> w a -> w b


class Functor f where
    <$>     :: (a -> b) -> f a -> f b

s
class Functor f => Applicative f where
    pure      ::    x           -> f x
    <*>       ::    f (a -> b)  -> f a   ->  f b    

--}

---------------------------------------------------------------------------------

-- Formally declaring it (rather than importing Mezzolenses)
class (Profunctor p, Functor f)    =>   Sieve  p f  |  p -> f  where
        sieve    ::     p  s  t    ->   s   ->   f  t



-- Re-examining our experimental type
data Multistar   f  a  b  s  t              =           Multistar    {  up   ::   a     ->    f b,
 
                                                                        down ::   f s   ->    t
                                                                     }


-- Now we construct a Profunctor, like we did before
instance Functor f => Profunctor (Multistar f a b) where
    dimap   h   g   (Multistar   u   d)     =           Multistar  u  (g . d . fmap h)




-- First, we make our Multistar Strong
-- We did a litte cheating, since we made our Functor a lot more powerful by making it an applicative and a comonad
--   --------->>>>> It used to be :         class  Profunctor p                              =>    Strong p  
--                  But now it is :         class (Profunctor p, Applicative f, Comonad f)   =>    Strong p
--   
instance   (Applicative f, Comonad f)   =>    Strong   (Multistar f a b) where
           first'   (Multistar   u   d)     =           Multistar  u  (extract  .  ((\y ->  ((d . pure . fst $ y ), snd y)) <$>)) 
           second'  (Multistar   u   d)     =           Multistar  u  (extract  .  ((\y ->  (fst y, (d . pure . snd $ y ))) <$>)) 




-- Now making a Sieve. The upstar portion of the multistar is irrelevant
instance   Applicative f   =>     Sieve  (Multistar f a b) f where
            sieve  (Multistar   _   d)      =           pure . d . pure



---------------------------------------------------------------------------------

-- REINVENTING!

-- This will help be our primary sieve functor
data   OpFunc a              =    OpFunc a 


-- Let's invent an alternative type, for the purposes of our Sieve
data   DownFunc a'           =    DownFunc a' 


-- We'll will be going from this type...
data   From                  =    From



-- And eventually end up with this type.... 
data   To                    =   To


---------------------------------------------------------------------------------

--First let's make our OpFunc a functor, since it will be represented in f
instance Functor OpFunc where
    fmap f (OpFunc x)        =     OpFunc (f x)



-- Now, doing the due diligence based on the functor constraints way up top...
instance Applicative OpFunc where
  pure x                     =     OpFunc x
  OpFunc y   <*>  OpFunc z   =     OpFunc (y z)



instance Comonad OpFunc where
    extract (OpFunc x)       =     x
    duplicate  x             =     OpFunc x
    extend     f             =     fmap f . duplicate



-- Re-inventing the functions from the vanilla MultiStar
-- Here is our Multistar variation of the contravariant function (from the vanilla)
epoch            ::    k           ->    From
epoch                        =     undefined



-- Here is the covariant function...
evolve           ::   To           ->    s'
evolve                       =     undefined



-- Here is a way to go up
outflow          ::   a            ->    OpFunc b 
outflow                      =     undefined



-- And this a way to go down...
supernova         ::  OpFunc From   ->    To
supernova                    =     undefined



---------------------------------------------------------------------------------


-- Defining the Profunctor 
multiFunctorP     ::        Multistar OpFunc  a  b  From  To  
multiFunctorP          =    dimap epoch evolve (Multistar outflow supernova)



-- And now for our Optic
multiOptic             ::   Multistar OpFunc a' b' a  b     ->    Multistar OpFunc a' b' From To
multiOptic        m    =    Multistar (up m) supernova



---------------------------------------------------------------------------------



-- Let's use the sieve and optic to obtain the resultant types we are really interested in, from the "left"...
multiSieveF             ::   To
multiSieveF             =    fst . extract . sieve (first' . multiOptic $ multiFunctorP) .  (,) From $ id 
  


-- As well as from the "right"...  
multiSieveR             ::   To
multiSieveR             =    snd . extract . sieve (second' . multiOptic $ multiFunctorP) . (,) id $ From


---------------------------------------------------------------------------------

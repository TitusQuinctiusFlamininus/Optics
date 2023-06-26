{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}


module MultistarStrongSieveOpticsFun where

import Control.Comonad
import Control.Lens.Combinators    (Profunctor, dimap            )
import Data.Profunctor.Strong      (Strong    , first', second'  )

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
type Optic p a b s t = p a b -> p s t


-- From the Mezzolens Haskell Package, we find this definition: 
https://hackage.haskell.org/package/mezzolens-0.0.0/docs/Mezzolens-Profunctor.html#t:OutPhantom


class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b                 <<-----
                                                             |-------- Either implement these two ...
  rmap  :: (b -> d) -> p a b -> p a d                 <<-----  

  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d     <<-------------- Or just this one


class (Profunctor p, Functor f) => Sieve p f | p -> f where
        sieve :: p a b -> a -> f b
  

class Profunctor p => Strong p where
        first'   ::  p  a  b   -> p  (a,  c)  (b,  c)
        second'  ::  p  a  b   -> p  (c,  a)  (c,  b)


-- As a convenience only so we can develop our example further than it normally is possible
class Functor w => Comonad w where
        extract   :: w a -> a
        duplicate :: w a -> w (w a)
        extend    :: (w a -> b) -> w a -> w b

--}

---------------------------------------------------------------------------------
-- Formally declaring it (rather than importing Mezzolenses)
class (Profunctor p, Functor f) => Sieve p f | p -> f where
        sieve :: p a b -> a -> f b


-- Re-examining our experimental type
data Multistar   f  a  b  s  t              =           Multistar    {  up   ::   a     ->    f b,
 
                                                                        down ::   f s   ->    t
                                                                     }


-- Now we construct a Profunctor, like we always do
instance Functor f => Profunctor (Multistar f a b) where
    dimap   h   g   (Multistar   u   d)     =           Multistar  u  (g . d . fmap h)




-- First, we make our Multistar Strong
-- We did a litte cheating, since we made our Functor a lot more powerful by making it an applicative
-- We also needed a way to dissociate the functorial context from the resultant strong tuple, in down
-- Without the additional comonadic context, it would not be possible to forme a strong Multistar
instance   (Applicative f, Comonad f)   =>    Strong   (Multistar f a b) where
           first'   (Multistar   u   d)     =           Multistar  u  (extract  .  ((\y ->  ((d . pure . fst $ y ), snd y)) <$>)) 
           second'  (Multistar   u   d)     =           Multistar  u  (extract  .  ((\y ->  (fst y, (d . pure . snd $ y ))) <$>)) 


-- Now making a Sieve
instance   Applicative f   =>     Sieve  (Multistar f a b) f where
            sieve  (Multistar   _   d)      =           pure . d . pure
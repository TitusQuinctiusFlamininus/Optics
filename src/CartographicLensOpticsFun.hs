{-# LANGUAGE RankNTypes #-}

module CartographicLensOpticsFun where 

import Control.Comonad             (Comonad, extract)
import Control.Lens.Combinators    (Profunctor, dimap)
import Data.Profunctor.Strong      (Strong, first', second')


{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up


class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b                 <<-----
                                                             |-------- Either implement these two ...
  rmap  :: (b -> d) -> p a b -> p a d                 <<-----  

  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d     <<-------------- Or just this one


where p is a Profunctor : 
type  Optic  p  a  b  s  t    =  p  a  b    ->   p  s  t

class Profunctor p => Strong p where
  first' :: p a b -> p (a, c) (b, c)
  second' :: p a b -> p (c, a) (c, b)


class (Strong p, Choice p) => Wandering p where
  wander :: Traversable f => p a b -> p (f a) (f b)

class Wandering p  =>  Cartographic p where
  map :: Functor f => p a b -> p (f a) (f b)


References: 
For Traversable, see: https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Traversable.html#t:Traversable
For Strong, see: https://hackage.haskell.org/package/mezzolens-0.0.0/docs/Mezzolens-Profunctor.html#t:Strong
For Choice, see: https://hackage.haskell.org/package/mezzolens-0.0.0/docs/Mezzolens-Profunctor.html#t:Choice

--}


---------------------------------------------------------------------------------

-- Defining a Vanilla definition
data  CartographicLens  a  b  s  t    =    CartLens {   find   ::  s       ->   a,
                                                        pin    ::  (b, s)  ->   t
                                                    }                                  

-- First making it a Profunctor:
instance Profunctor (CartographicLens a b) where
    dimap  h    g    (CartLens q  r )    =  CartLens  ( q . h ) (\y ->   g . r $ (fst y, (h $ snd y)) )


-- First we make strengthen the cartographic profunctor: 
-- See this as a reference: https://github.com/TitusQuinctiusFlamininus/Optics/blob/main/src/LensCartesianOpticalFun.hs
instance Strong (CartographicLens  a  b) where
  first'  (CartLens q  r ) = CartLens (q . fst)  (\y -> (r (fst y, fst . snd $ y), snd . snd $ y))
  second' (CartLens q  r ) = CartLens (q . snd)  (\y -> (fst . snd $ y, r (fst y, snd . snd $ y)))

-- We know that we cannot make this profunctor Choice; See: https://github.com/TitusQuinctiusFlamininus/Optics/blob/main/src/LensCoCartesianOpticalFun.hs

-- For the sake of continuity, we can eliminate the need for Choice and modify the Wandering Definition slightly to be: 
class (Profunctor p, Strong p) => Wandering p where
   wander' :: (Applicative f, Comonad f) => p a b -> p (f a) (f b)

-- We need to make it both an applicative and a comonad for later reasons
-- Then we make it Wandering:
instance Wandering (CartographicLens a b) where
  wander' (CartLens q  r) = CartLens (extract . fmap q) (\y -> pure $ r (fst y, extract . snd $ y))

-- So then we also change the definition of Cartographic as well:
class (Profunctor p, Wandering p) => Cartographic p where
   map' :: (Applicative f, Comonad f) => p a b -> p (f a) (f b)


-- Then we make it a cartographic: 
instance Cartographic (CartographicLens a b) where  
  map' = wander'
                  

---------------------------------------------------------------------------------

-- Revisiting the types from the Vanilla Lens but simplified a bit:
newtype  Composite    a   =   Composite    a

newtype  NewComposite b   =   NewComposite b

---------------------------------------------------------------------------------
-- The functions from before....

zig   ::   m    ->  Composite a
zig   = undefined

zag   ::   NewComposite b  ->   n
zag   = undefined

focus  ::   Composite a  ->   a
focus  = undefined

comp   ::   (b, Composite a)  ->   NewComposite b
comp   = undefined

---------------------------------------------------------------------------------
-- Ok, the profunctor we can form:
telescopicP   :: CartographicLens a b x y 
telescopicP   = CartLens (focus . zig) (\z  -> zag . comp $ (fst z, zig $ snd z))

first'TelescopicP   ::   CartographicLens a b (a, c) (b, c)
first'TelescopicP            =   first' telescopicP

second'TelescopicP  ::   CartographicLens a b (c, a) (c, b)
second'TelescopicP           =   second' telescopicP

wanderingFirst'TelescopicP  :: (Applicative f, Comonad f) => CartographicLens a b (f (a, c)) (f (b, c))
wanderingFirst'TelescopicP   =   wander' first'TelescopicP

wanderingSecond'TelescopicP  :: (Applicative f, Comonad f) => CartographicLens a b (f (c, a)) (f (c, b))
wanderingSecond'TelescopicP  =   wander' second'TelescopicP

---------------------------------------------------------------------------------
-- Moving to the Optics: 

basicOpticP  ::   CartographicLens a b s t  ->  CartographicLens s t s t
basicOpticP _         =   CartLens id fst

wanderedBasicOpticP  ::  (Applicative f, Comonad f) => CartographicLens a b s t  ->  CartographicLens (f s) (f t) s t
wanderedBasicOpticP _ =   CartLens pure (extract . fst)

-- Deriving the Cartographic Optics: 

-- Left side:   f (s, c)      -> a 
-- Second side: (b, f (s, c)) -> f (t, c)
wanderingFirst'OpticP ::  (Applicative f, Comonad f) => CartographicLens a b s t -> CartographicLens a b (f (s, c)) (f (t, c))
wanderingFirst'OpticP  (CartLens k v)  =   CartLens leftSide rightSide
  where leftSide  =  k . fst . extract
        rightSide = \w -> pure ((v (fst w, fst . extract . snd $ w)), snd . extract . snd $ w)  


-- Left side:  f (c, s)      -> a
-- Right side: (b, f (c, s)) -> f (c, t)
wanderingSecond'OpticP ::  (Applicative f, Comonad f) => CartographicLens a b s t -> CartographicLens a b (f (c, s)) (f (c, t))
wanderingSecond'OpticP  (CartLens k v)  =   CartLens leftSide rightSide 
  where leftSide  = k . snd . extract
        rightSide = (\w -> pure (fst . extract . snd $ w, v(fst w, snd . extract . snd $ w)))

---------------------------------------------------------------------------------
-- Just for fun, left's try and derive some stranger cartographic optics: 

-- Left side : f (s, c)      -> a
-- Right side: (b, f (s, c)) -> f (t, c)
lostFirst'OpticP ::  (Applicative f, Comonad f) => CartographicLens (f a) (f b) s t -> CartographicLens a b (f (s, c)) (f (t, c))
lostFirst'OpticP  (CartLens k v)  =   CartLens leftSide rightSide
  where leftSide  = extract . k . fst . extract 
        rightSide = (\w ->  pure (v (pure . fst $ w, fst . extract . snd $ w), snd . extract . snd $ w))


-- Left Side:  f (c, s)      -> a
-- Right Side: (b, f (c, s)) -> f (c, t)
lostSecond'OpticP ::  (Applicative f, Comonad f) => CartographicLens (f a) (f b) s t -> CartographicLens a b (f (c, s)) (f (c, t))
lostSecond'OpticP (CartLens k v)  =   CartLens leftSide rightSide
  where leftSide  = extract . k . snd . extract 
        rightSide = (\w ->  pure (fst . extract . snd $ w, v (pure . fst $  w, snd . extract . snd $ w)))


---------------------------------------------------------------------------------

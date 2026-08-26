module TFunctor.Lens_TFunctorOpticsFun where

{-

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

A TFunctor can be visualized as: 


                         tmap
                          │
                          │
              ┌───────────┴───────────┐
              │                       │
           a' -> b               c -> k
              │                       │
              ▼                       ▼
           ┌─────┐                 ┌─────┐
           │  b  │                 │  k  │
           └──┬──┘                 └──┬──┘
              │                       │
              │                       │
              │       p               │
              └──────► b ───────► c ◄─┘
                         │
                         │
                         │  k -> c -> e
                         ▼
                         e


             p b c
               │
               │
               │  tmap
               ▼
             p a' e


  ┌─────────────────────────────────────────────────────────┐
  │                                                         │
  │  tmap :: (a' -> b)                                      │
  │       -> (k -> c -> e)                                  │
  │       -> (c -> k)                                       │
  │       -> p b c                                          │
  │       -> p a' e                                         │
  │                                                         │
  └─────────────────────────────────────────────────────────┘

  The transformation can be viewed as:

        a' ──────► b
                    │
                    │
                  p b c
                    │
                    ▼
                    c
                    │
                    ▼
                    k
                    │
                    ▼
                    e

        giving

                  p b c
                    │
                   tmap
                    ▼
                  p a' e

So defining:
class TFunctor p where
   tmap :: (a' -> b) -> (k -> c -> e) -> (c -> k) -> p b c -> p a' e


-}

---------------------------------------------------------------------------------

-- Formally Defining a Tfunctor
class TFunctor p where
  tmap :: (a' -> b) -> (k -> c -> e) -> (c -> k) -> p b c -> p a' e

-- But first, let's roll our own Lens
data TFunctorLens a b s t         = TLens {  r  :: s         ->     a, 

                                             r' :: (b, s)    ->     t 
                                           }

-- We end up with something close to a profunctor instance
-- The idea is to feed our m function 2 parameters, one of which is a sub-component in the other, so we extract it as y
-- The left hand side is identical to that of the regular profunctor, since most morphing occurs after the internal transformer does its thing

instance TFunctor (TFunctorLens s t) where
    tmap k m n (TLens l e) = TLens (l . k) $ \x -> let y = e (fst x, k (snd x)) in m (n y) y


---------------------------------------------------------------------------------

-- TFunctor Optics definition (where p is a TFunctor)
type TFunctorOptic p a b s t = p  a  b   ->  p  s  t

-- Creating a Generalized TFunctor Optic, but we need to supply the lens functions
lensTFuncOptic :: (s -> a) -> ((b, s) -> t) -> TFunctorLens  a b a b  ->  TFunctorLens a b s t
lensTFuncOptic x y (TLens g h)  = TLens x y
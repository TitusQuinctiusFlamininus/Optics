module TFunctor.LensOpticsFun where


import Control.Lens.Combinators (Profunctor, dimap)

{-

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

--First we define a regular Profunctor 
class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b                 <<-----
                                                             |-------- Either implement these two ...
  rmap  :: (b -> d) -> p a b -> p a d                 <<-----  

  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d     <<-------------- Or just this one

And a TFunctor can be visualized as: 


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

-}


---------------------------------------------------------------------------------
-- Defining a Tfunctor (where p is a Profunctor)
class TFunctor p where
  tmap :: (a' -> b) -> (k -> c -> e) -> (c -> k) -> p b c -> p a' e

-- But first, let's roll our own Lens
data TFunctorLens a b s t         = TLens {  look :: s         ->     a, 

                                             edit :: (b, s)    ->     t 
                                           }

-- Turning our custom type into a Profunctor
instance Profunctor (TFunctorLens s t) where
    dimap h g (TLens l e)   =   TLens (l . h) (\x  -> g . e $ (fst x, h . snd $ x))


--For Convenience: remember how we defined a Tfunctor 
-- class TFunctor p where
--   tmap :: (a' -> b) -> (k -> c -> e) -> (c -> k) -> p b c -> p a' e


-- Now lets make it into a TFunctor 
instance TFunctor (TFunctorLens s t) where
    tmap k m n (TLens l e) = TLens (l . k) $ \x -> let y = e (fst x, k (snd x)) in m (n y) y
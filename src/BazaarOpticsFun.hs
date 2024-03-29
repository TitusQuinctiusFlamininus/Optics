{-# LANGUAGE RankNTypes #-}

module BazaarOpticsFun where

import Control.Lens.Combinators (Profunctor, dimap)

{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up


class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b                 <<-----
                                                             |-------- Either implement these two ...
  rmap  :: (b -> d) -> p a b -> p a d                 <<-----  

  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d     <<-------------- Or just this one


where p is a Profunctor : 
type    Optic  p  a  b  s  t =  p  a  b  ->  p  s  t


--}

---------------------------------------------------------------------------------

-- Let's formally define our wierd Bizzare type 
newtype Bazaar p  a  b  t    =  Bazaar { runBazaar :: forall f. Applicative f => p a (f b) -> f t }


--Attempting to make the Bazaar a Profunctor
instance Profunctor (Bazaar p a) where
    dimap  h  g  (Bazaar z)  = Bazaar undefined
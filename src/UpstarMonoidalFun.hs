module UpstarMonoidalFun where

import Control.Lens.Combinators (Profunctor, dimap)


{--

To understand this better, read from top to bottom, in the style 
that the code was slowly built up

class Profunctor p where
  lmap        :: (c -> a) -> p a b    -> p c b
  rmap        :: (b -> d) -> p a b    -> p a d
  dimap       :: (c -> a) -> (b -> d) -> p a b -> p c d


class Profunctor p => Monoidal p where
  par         :: p a b    -> p c d    -> p (a, c) (b, d)
  empty       :: p () ()


class Functor f where
    fmap      :: (a -> b) -> f a -> f b


class functor f => Applicative f where
    pure      :: x           -> f x
    <*>       :: f (a -> b)  -> f a   ->  f b      <<<<------ starship

--}



  ---------------------------------------------------------------------------------


-- Could not find this definition in the combinators lib, nor in another haskell library , so i'll just spell it out here formally for the typechecker
class Profunctor  p => Monoidal p where
  par        ::   p a b     ->    p c d     -> p (a, c) (b, d)
  empty      ::   p () ()



-- Ok, Let us invent our own Upstar
newtype MonoStar f  a  b                     =   MonoidalStar { unstar ::  a  ->  f b }



-- Alright, making it a Profunctor...
instance Functor f =>  Profunctor (MonoStar f) where
    dimap   h   g   (MonoidalStar r)         =   MonoidalStar ( fmap g . r . h ) 



-- And now making it Monoidal...

-- Explanation  : 
--        ------->>> First, we take the first tuple type and apply it to v; Then fmap that to a tuple function; We end up with a partial tuple function associated with a functor context
--                   Next, we provide type d to the second profunctor's unstar; Finally, we match both with the starship operator
--        ------->>> For empty, we have to associate a functor with our resultant type, which implies an applicative lift
instance Applicative f =>  Monoidal  (MonoStar f)  where
    par   (MonoidalStar v) (MonoidalStar w)  =   MonoidalStar (\x  ->  ((,) <$> (v . fst $ x)) <*>  (w . snd $ x))
    empty                                    =   MonoidalStar pure 



  ---------------------------------------------------------------------------------


-- Let's invent some Billing Types for Household services in a (very) contrived example


-- Here's are the kinds of service one can get from a Service Provider
data  Service       =  Phone      |   Internet



-- When you pay for a Service, you get one of these types
data  Bill  b       =  Bill  b


-- However: 
-- A Service Provider could still be operating like in the 80s...
data PaperReceipt  



-- Or they could be hipsters and keep payment records in crypto form...
data BitcoinReceipt


---------------------------------------------------------------------------------

-- How about some functions to go along with these types

cashRegister      ::    Service       ->    f (Bill PaperReceipt)
cashRegister         = undefined


chainBilling      ::    Service       ->    f (Bill BitcoinReceipt)
chainBilling         = undefined


---------------------------------------------------------------------------------

-- Time for the more interesting part: How can we use the Monoidal 


-- Here's a General Monoidal Profunctor for the masses, we just need the appropriate means of billing...
monoP          :: Applicative f =>  (Service   ->   f (Bill b))  ->  MonoStar f  Service  (Bill b)
monoP                      =  dimap id id . MonoidalStar




-- How about we bundle everything from our Service Providers ....
-- Just make sure we have pairs for each service reflecting billing for each month
monoTax    :: Applicative f =>  [Service]  ->  [Service]   ->   [f (Bill PaperReceipt, Bill BitcoinReceipt)]
monoTax  _      []      =  []
monoTax  []     _       =  []
monoTax  s      s'      =  ((unstar . par (monoP cashRegister) $ (monoP chainBilling)) . (,) (head s) $ (head s')) : monoTax (tail s) (tail s')




-- If we prefer, we can organize all our bills in one place for next year's taxes
accountant     :: Applicative f =>  [f (Bill a, Bill b)]  -> f ([Bill a], [Bill b])
accountant                  =  head . ((\x  -> (\s  -> ([fst s], [snd s])) <$> x) <$>)



---------------------------------------------------------------------------------

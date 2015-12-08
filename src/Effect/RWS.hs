{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE GADTSyntax #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE UndecidableInstances #-}

module Effect.RWS where

import Data.Codensity
import Typeclass.Coproduct
import Typeclass.TermMonad
import Typeclass.TermAlgebra

data RWS r w s k where
    Ask  :: (r -> k) -> RWS r w s k
    Tell ::  w -> k  -> RWS r w s k
    Get  :: (s -> k) -> RWS r w s k
    Put  ::  s -> k  -> RWS r w s k

instance Functor (RWS r w s) where
    fmap f (Ask g)    = Ask (f . g)
    fmap f (Tell w k) = Tell w (f k) 
    fmap f (Put s k)  = Put s (f k) 
    fmap f (Get k)    = Get (f . k)

newtype RWSCarrier r w s m a = RWSC {unRWS :: r -> s -> m (a, s, w)}

instance Functor m => Functor (RWSCarrier r w s m) where
    fmap f x = RWSC (fmap (fmap (fmap (lift f))) (unRWS x))
        where lift :: (a -> b) -> ((a, s, w) -> (b, s, w))
              lift f (a, b, c) = (f a, b, c)

instance (TermMonad m f, Monoid w) => TermAlgebra (RWSCarrier r w s m) (RWS r w s + f) where
    var = RWSC . genRWS
    con = RWSC . (algRWS \/ conRWS) . fmap unRWS

runRWS :: (Monoid w, TermMonad m f) => Codensity (RWSCarrier r w s m) a -> (r -> s -> m (a, s, w))
runRWS = unRWS . runCod var

conRWS :: TermMonad m f => f (r -> s -> m (a, s, w)) -> r -> s -> m (a, s, w)
conRWS op r s = con (fmap (\m -> m r s) op)

genRWS :: (Monoid w, TermMonad m f) => a -> (r -> s -> m (a, s, w))
genRWS a = (\_ s -> return (a, s, mempty)) -- TODO: Remove s

algRWS :: RWS r w s (r -> s -> m (a, s, w)) -> (r -> s -> m (a, s, w))
algRWS (Ask g) r s = g r r s
algRWS (Tell w k) r s = _algTell -- TODO: Find implementation
algRWS (Get k) r s = k s r s
algRWS (Put s' k) r s = k s' r


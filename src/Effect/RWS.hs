{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE GADTSyntax #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE UndecidableInstances #-}
{-#LANGUAGE AllowAmbiguousTypes #-}
{-#LANGUAGE ScopedTypeVariables #-}

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

ask :: forall m f r w s. (TermMonad m f, RWS r w s :< f) => m r
ask = inject (Ask var :: RWS r w s (m r))

tell :: forall m f r w s. (TermMonad m f, RWS r w s :< f) => w -> m ()
tell w = inject (Tell w (var ()) :: RWS r w s (m ()))

get :: forall m f r w s. (TermMonad m f, RWS r w s :< f) => m s
get = inject (Get var :: RWS r w s (m s))

put :: forall m f r w s.(TermMonad m f, RWS r w s :< f) => s -> m ()
put s = inject (Put s (var ()) :: RWS r w s (m ()))

newtype RWSCarrier r w s m a = RWSC {unRWS :: r -> s -> m (w, a)}

instance Functor m => Functor (RWSCarrier r w s m) where
    fmap f x = RWSC (fmap (fmap (fmap (fmap f))) (unRWS x))

instance (TermMonad m f, Monoid w) => TermAlgebra (RWSCarrier r w s m) (RWS r w s + f) where
    var = RWSC . genRWS
    con = RWSC . (algRWS \/ conRWS) . fmap unRWS

runRWS :: (Monoid w, TermMonad m f) => Codensity (RWSCarrier r w s m) a -> (r -> s -> m (w, a))
runRWS = unRWS . runCod var

conRWS :: TermMonad m f => f (r -> s -> m (w, a)) -> r -> s -> m (w, a)
conRWS op r s = con (fmap (\m -> m r s) op)

genRWS :: (Monoid w, TermMonad m f) => a -> (r -> s -> m (w, a))
genRWS a = \_ _ -> return (mempty, a)

algRWS :: (Monoid w, TermMonad m f) => RWS r w s (r -> s -> m (w, a)) -> (r -> s -> m (w, a))
algRWS (Ask g) r s = g r r s
algRWS (Tell w k) r s = let a = k r s
                        in a >>= \(w', x) -> return (w' `mappend` w, x)
algRWS (Get g) r s = g s r s
algRWS (Put s' k) r _ = k r s'

{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE GADTSyntax #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE UndecidableInstances #-}
{-#LANGUAGE AllowAmbiguousTypes #-}
{-#LANGUAGE DeriveFunctor #-}
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

instance {-# OVERLAPPING #-} (Functor f, a ~ a', b ~ b', c ~ c') => RWS a b c :< (RWS a' b' c' + f) where
    inj = Inl
    prj (Inl fa) = Just fa
    prj (Inr _) = Nothing

ask :: forall m f r w s. (TermMonad m f, RWS r w s :< f) => m r
ask = inject (Ask var :: RWS r w s (m r))

tell :: forall m f r w s. (TermMonad m f, RWS r w s :< f) => w -> m ()
tell w = inject (Tell w (var ()) :: RWS r w s (m ()))

get :: forall m f r w s. (TermMonad m f, RWS r w s :< f) => m s
get = inject (Get var :: RWS r w s (m s))

put :: forall m f r w s.(TermMonad m f, RWS r w s :< f) => s -> m ()
put s = inject (Put s (var ()) :: RWS r w s (m ()))

newtype RWSCarrier r w s m a = RWSC {unRWS :: r -> s -> m (a, w, s)} deriving (Functor)

instance (TermMonad m f, Monoid w) => TermAlgebra (RWSCarrier r w s m) (RWS r w s + f) where
    var = RWSC . genRWS
    con = RWSC . (algRWS \/ conRWS) . fmap unRWS

runRWS :: (Monoid w, TermMonad m f) => Codensity (RWSCarrier r w s m) a -> r -> s -> m (a, w, s)
runRWS = unRWS . runCod var

conRWS :: TermMonad m f => f (r -> s -> m (a, w, s)) -> r -> s -> m (a, w, s)
conRWS op r s = con (fmap (\m -> m r s) op)

genRWS :: (Monoid w, TermMonad m f) => a -> r -> s -> m (a, w, s)
genRWS a _ s = return (a, mempty, s)

algRWS :: (Monoid w, TermMonad m f) => RWS r w s (r -> s -> m (a, w, s)) -> r -> s -> m (a, w, s)
algRWS (Ask g) r s = g r r s
algRWS (Tell w k) r s = let a = k r s
                        in a >>= \(x, w', s) -> return (x, w' `mappend` w, s)
algRWS (Get g) r s = g s r s
algRWS (Put s' k) r _ = k r s'

newtype LocalRWSCarrier m r w s a = LRWSC {unLRWS :: (r -> r) -> m (a, w, s)} deriving (Functor)

instance (Monoid w, Functor f, TermMonad m (RWS r w s + f)) => TermAlgebra (LocalRWSCarrier m r w s) (RWS r w s + f) where
  var = LRWSC . genLocal
  con = LRWSC . (algLocal \/ conLocal) . fmap unLRWS

genLocal :: (TermMonad m f, Monoid w) => a -> (r -> r) -> m (a, w, s)
genLocal a = const (return (a, mempty, _))

algLocal :: (Functor f, TermMonad m (RWS r w s + f)) => RWS r w s ((r -> r) -> m (a, w, s)) -> (r -> r) -> m (a, w, s)
algLocal (Ask k) g = con(Inl(Ask(\e -> k (g e) g)))
algLocal (Tell w k) g = con(Inl(Tell w (k g)))
algLocal (Get k) g = con(Inl(Get(\s -> k s g)))
algLocal (Put s' k) g = con(Inl(Put s' (k g)))

conLocal :: (Functor f, Functor g, TermAlgebra m (g + f)) => f ((e -> e) -> m a) -> (e -> e) -> m a
conLocal a e = con (Inr (fmap (\g -> g e) a))

local :: (Functor f, TermMonad m (RWS r w s + f), Monoid w) => Codensity (LocalRWSCarrier m r w s) a -> (r -> r) -> m (a, w, s)
local = unLRWS . runCod var

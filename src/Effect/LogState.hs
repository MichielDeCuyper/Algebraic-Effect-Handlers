{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTSyntax            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Effect.LogState where

import           Data.Codensity
import           Effect.State
import           Effect.Writer
import           Typeclass.Coproduct
import           Typeclass.TermAlgebra
import           Typeclass.TermMonad

newtype LogStateCarrier s m a = LSC {unLSC :: s -> m a}

instance Functor m => Functor (LogStateCarrier s m) where
    fmap f x = LSC (fmap (fmap f) (unLSC x))

--instance (Functor f, Functor g, TermMonad m f, TermAlgebra m (Writer String + g)) => TermAlgebra (LogStateCarrier s m) (State s + f) where
--    var = LSC . genState
--    con = LSC . (algLogState \/ conLogState) . fmap unLSC

instance (Functor f, TermMonad m (Writer String + f)) => TermAlgebra (LogStateCarrier s m) (State s + f) where
    var = LSC . genState
    con = LSC . (algLogState \/ conLogState) . fmap unLSC

algLogState :: (Functor g, TermMonad m (Writer String + g)) => State s (s -> m a) -> s -> m a
algLogState (Put s' k) s = con(Inl(Tell "put" (k s')))
algLogState (Get k) s = k s s

runLogState :: (Functor f, TermMonad m (Writer String +f)) => Codensity (LogStateCarrier s m) a -> s -> m a
runLogState = unLSC . runCod var

conLogState :: (Functor f, Functor g, TermAlgebra m (g + f)) => f (s -> m a) -> s -> m a
conLogState op s = con(Inr(fmap (\m -> m s) op))

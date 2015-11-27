{-#LANGUAGE TypeOperators, GADTSyntax, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, IncoherentInstances, AllowAmbiguousTypes#-}

module Effect.LogState where

import Data.Codensity
import Effect.State
import Effect.Writer
import Typeclass.TermAlgebra
import Typeclass.TermMonad
import Typeclass.Coproduct

newtype LogStateCarrier s m a = LSC {unLSC :: s -> m a}

instance Functor m => Functor (LogStateCarrier s m) where
    fmap f x = LSC (fmap (fmap f) (unLSC x)) 

instance (Functor f, Functor g, TermMonad m f, TermAlgebra m (Writer String + g)) => TermAlgebra (LogStateCarrier s m) (State s + f) where
    var = LSC . genState
    con = LSC . (algLogState \/ conLogState) . fmap unLSC

algLogState :: (Functor g, TermMonad m (Writer String + g)) => State s (s -> m a) -> s -> m a
algLogState (Put s' k) s = tell "put" (k s')
algLogState (Get k) s = k s s

runLogState :: (Functor f, TermMonad m (Writer String + f)) => Codensity (LogStateCarrier s m) a -> s -> m a
runLogState = unLSC . runCod var

conLogState :: TermAlgebra m f => f (s -> m a) -> s -> m a
conLogState op s = con (fmap (\m -> m s) op)
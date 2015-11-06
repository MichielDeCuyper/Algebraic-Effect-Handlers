{-#LANGUAGE TypeOperators, GADTSyntax, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances, IncoherentInstances#-}

module Effect.LogState where

import Data.Free
import Data.Codensity
import Effect.State
import Effect.Void
import Typeclass.TermAlgebra
import Typeclass.TermMonad
import Typeclass.Coproduct

data Writer w k where
    Tell :: w -> k -> Writer w k

instance Functor (Writer w) where
    fmap f (Tell w k) = Tell w (f k) 

newtype LogStateCarrier s m a = LSC {unLSC :: s -> m a}

instance Functor m => Functor (LogStateCarrier s m) where
    fmap f x = LSC (fmap (fmap f) (unLSC x)) 

instance TermMonad m (Writer String + Void) => TermAlgebra (LogStateCarrier s m) (State s) where
    var = LSC . genState
    con = LSC . algLogState . fmap unLSC

algLogState :: TermAlgebra m (Writer String + g) => State s (s -> m a) -> s -> m a
algLogState (Put s' k) s = con (Inl (Tell "put" (k s')))
algLogState (Get k) s = k s s

runLogState :: TermMonad m (Writer String + Void) => Codensity (LogStateCarrier s m) a -> (s -> m a)
runLogState = unLSC . runCod var
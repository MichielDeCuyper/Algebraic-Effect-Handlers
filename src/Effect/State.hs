{-# LANGUAGE GADTSyntax, TypeOperators, FlexibleInstances, MultiParamTypeClasses,
UndecidableInstances#-}

module Effect.State (State, StateCarrier, handleState) where

import Data.Free
import Data.Codensity
import Typeclass.Coproduct
import Typeclass.TermAlgebra
import Typeclass.TermMonad

data State s k where
    Put :: s -> k -> State s k
    Get :: (s -> k) -> State s k

instance Functor (State s) where
    fmap f (Put s k) = Put s (f k) 
    fmap f (Get k) = Get (f . k)

newtype StateCarrier s m a = SC {unSC :: s -> m a}

instance Functor m => Functor (StateCarrier s m) where
	fmap f x = SC (fmap (fmap f) (unSC x))

instance TermMonad m f => TermAlgebra (StateCarrier s m) (State s + f) where
	con = SC . (algState \/ con) . fmap unSC
	var = SC . genState

handleState :: TermMonad m f => Codensity (StateCarrier s m) a -> (s -> m a)
handleState = unSC . runCod var

algState :: TermMonad m f => State s (s -> m a) -> (s -> m a)
algState (Put s' k) s = k s'
algState (Get k) s = k s s
 
genState :: TermMonad m f => a -> (s -> m a)
genState x = const (var x)
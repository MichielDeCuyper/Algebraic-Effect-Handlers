{-# LANGUAGE GADTSyntax, TypeOperators, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts#-}

module Effect.State (State(Get, Put), get, put, StateCarrier, runState, con, var, algState, genState, conState, example) where

import Effect.Void
import Data.Codensity
import Typeclass.Coproduct
import Typeclass.TermAlgebra
import Typeclass.TermMonad

data State s k where
    Put :: s -> k -> State s k
    Get :: (s -> k) -> State s k

get :: (TermMonad h f, State s :< f) => h s
get = inject (Get var)

put :: (TermMonad h f, State s :< f) => s -> h ()
put s = inject (Put s (var ()))

instance Functor (State s) where
    fmap f (Put s k) = Put s (f k) 
    fmap f (Get k) = Get (f . k)

newtype StateCarrier s m a = SC {unSC :: s -> m a}

instance Functor m => Functor (StateCarrier s m) where
    fmap f x = SC (fmap (fmap f) (unSC x))

instance TermMonad m f => TermAlgebra (StateCarrier s m) (State s + f) where
    con = SC . (algState \/ conState) . fmap unSC
    var = SC . genState

runState :: TermMonad m f => Codensity (StateCarrier s m) a -> s -> m a
runState = unSC . runCod var

algState :: TermMonad m f => State s (s -> m a) -> s -> m a
algState (Put s' k) s = k s'
algState (Get k) s = k s s
 
genState :: TermMonad m f => a -> s -> m a
genState x = const (var x) -- == \_ -> var x

conState :: (Functor g, TermAlgebra m g) => g (s -> m a) -> s -> m a
conState op s = con (fmap (\m -> m s) op)

example :: TermMonad h (State Int + Void) => Int -> h Int
example n
    | n <= 0 = get
    | otherwise = do a <- get 
                     put (a+n) 
                     example (n-1)



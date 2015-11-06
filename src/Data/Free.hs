{-# LANGUAGE GADTSyntax #-}
{-|
Module: Data.Free
Description: The Free Monad
Copyright: (c) Michiel De Cuyper 2015
License: MIT
Maintainer: Michiel.DeCuyper@student.kuleuven.be
Stability: Experimental
Portability: POSIX

This module contains the definition of the Free Monad.

Free monads are a core concept in Algebraic Effect Handlers, as they denote the tree of effects
-}
module Data.Free (Free(Var, Con), fold) where

-- | The free monad
-- The @Var@ constructor represents a variable
--
-- The @Con@ constructor represents a computation
data Free f a where
    Var :: a -> Free f a
    Con :: f (Free f a) -> Free f a 

instance Functor f => Functor (Free f) where
    fmap f (Var x) = Var (f x)
    fmap f (Con op) = Con $ fmap (fmap f) op 

instance Functor f => Applicative (Free f) where
     pure = Var
     Var f <*> s = fmap f s
     Con f <*> s = Con (fmap (<*> s) f)

instance Functor f =>  Monad (Free f) where
    return = pure
    m >>= f = fold Con f m

-- | TODO
-- 
fold :: Functor f => (f b -> b) -> (a -> b) -> Free f a -> b
fold alg gen (Var x) = gen x
fold alg gen (Con op) = alg (fmap (fold alg gen) op)

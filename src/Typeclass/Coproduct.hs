{-# LANGUAGE TypeOperators, GADTSyntax #-}
{-|
Module: Typeclass.Coproduct
Description: Coproduct
Copyright: (c) Michiel De Cuyper 2015
License: MIT
Maintainer: Michiel.DeCuyper@student.kuleuven.be
Stability: Experimental
Portability: POSIX

-}
module Typeclass.Coproduct (type (+), (\/) ) where

-- | Coproduct
--  The coproduct of two datatypes f and g is denoted as @f + g@
data (+) f g a where
    Inl :: f a -> (f + g) a
    Inr :: g a -> (f + g) a

instance (Functor f, Functor g) => Functor (f + g) where
    fmap f (Inl s) = Inl (fmap f s) 
    fmap f (Inr s) = Inr (fmap f s)

(\/) :: (f b -> b) -> (g b -> b) -> ((f + g) b -> b)
(\/) algF algG (Inl x) = algF x
(\/) algF algG (Inr x) = algG x
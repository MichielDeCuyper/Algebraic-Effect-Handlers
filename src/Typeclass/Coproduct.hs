{-# LANGUAGE TypeOperators, GADTSyntax, MultiParamTypeClasses, FlexibleInstances #-}
{-|
Module: Typeclass.Coproduct
Description: Coproduct
Copyright: (c) Michiel De Cuyper 2015
License: MIT
Maintainer: Michiel.DeCuyper@student.kuleuven.be
Stability: Experimental
Portability: POSIX

-}
module Typeclass.Coproduct (type (+) (Inl, Inr), (\/), (:<), inject ) where

import Data.Free

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

class (Functor sub, Functor sup) => sub :< sup where
    inj :: sub a -> sup a
    prj :: sup a -> Maybe (sub a)

instance Functor f => f :< f where
    inj = id
    prj = Just

instance (Functor f, Functor g) => f :< (f + g) where
    inj = Inl
    prj (Inl fa) = Just fa
    prj _ = Nothing

instance (Functor f, g :< sig) => g :< (f + sig) where
    inj = Inr . inj
    prj (Inr ga) = prj ga
    prj _ = Nothing

inject :: (sub :< sup) => sub (Free sup a) -> Free sup a
inject = Con . inj

project :: (sub :< sup) => Free sup a -> Maybe (sub (Free sup a))
project (Con s) = prj s
project _ = Nothing






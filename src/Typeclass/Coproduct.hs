{-# LANGUAGE TypeOperators, GADTSyntax #-}

module Typeclass.Coproduct ( (+) ) where

data (+) f g a where
    Inl :: f a -> (f + g) a
    Inr :: g a -> (f + g) a

instance (Functor f, Functor g) => Functor (f + g) where
    fmap f (Inl s) = Inl (fmap f s) 
    fmap f (Inr s) = Inr (fmap f s)
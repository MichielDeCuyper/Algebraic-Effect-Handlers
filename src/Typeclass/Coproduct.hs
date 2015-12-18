{-# LANGUAGE TypeOperators, GADTSyntax, MultiParamTypeClasses, FlexibleInstances #-}
{-|
Module: Typeclass.Coproduct
Description: Coproduct
Copyright: (c) Michiel De Cuyper 2015
License: MIT
Maintainer: Michiel.DeCuyper@student.kuleuven.be
Stability: Experimental
-}
module Typeclass.Coproduct (type (+) (Inl, Inr), (\/), (:<), inject ) where

import Typeclass.TermAlgebra
import Typeclass.TermMonad
-- | Coproduct
--  The coproduct of two datatypes f and g is denoted as @f + g@
data (+) f g a where
    Inl :: f a -> (f + g) a
    Inr :: g a -> (f + g) a

infixr 5 +

instance (Functor f, Functor g) => Functor (f + g) where
    fmap f (Inl s) = Inl (fmap f s)
    fmap f (Inr s) = Inr (fmap f s)

-- | The junction operator
--
-- The junction operator dispenses two functions @left :: f b -> b@ and @right :: g b -> b@ over a composed datatype @f + g@
--
-- The function @left@ gets applied when the value of the composed datatype is of type @f@ (i.e. the left part of the coproduct).
--
-- The function @right@ gets applied when the value of the composed datatype is of type @g@ (i.e. the right part of the coproduct).
(\/) :: (f b -> b) -> (g b -> b) -> ((f + g) b -> b)
(\/) algF algG (Inl x) = algF x
(\/) algF algG (Inr x) = algG x

class (Functor sub, Functor sup) => sub :< sup where
    inj :: sub a -> sup a
    prj :: sup a -> Maybe (sub a)

instance {-# OVERLAPPING #-} Functor f => f :< f where
    inj = id
    prj = Just

instance {-# OVERLAPPING #-} (Functor f, Functor g) => f :< (f + g) where
    inj = Inl
    prj (Inl fa) = Just fa
    prj _ = Nothing

instance {-# OVERLAPPING #-} (Functor f, g :< sig) => g :< (f + sig) where
    inj = Inr . inj
    prj (Inr ga) = prj ga
    prj _ = Nothing

inject :: (TermAlgebra h f, sub :< f) => sub (h a) -> h a
inject = con . inj

-- NOTE Is this needed?
--project :: (sub :< sup) => Free sup a -> Maybe (sub (Free sup a))
--project (Con s) = prj s
--project _ = Nothing

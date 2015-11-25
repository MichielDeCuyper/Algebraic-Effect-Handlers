{-#LANGUAGE GADTSyntax, RankNTypes, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances#-}

module Typeclass.TermAlgebra(TermAlgebra(var, con)) where

class Functor f => TermAlgebra h f | h -> f where
    var :: forall a . a -> h a
    con :: forall a . f (h a) -> h a

{-#LANGUAGE GADTSyntax, RankNTypes, MultiParamTypeClasses, 
FunctionalDependencies, FlexibleInstances, UndecidableInstances#-}

module Typeclass.TermAlgebra(TermAlgebra) where

import Data.Free

class Functor f => TermAlgebra h f | h -> f where
    var :: forall a . a -> h a
    con :: forall a . f (h a) -> h a

instance Functor f => TermAlgebra (Free f) f where
    var = Var
    con = Con


{-#LANGUAGE GADTSyntax, TypeOperators, MultiParamTypeClasses, UndecidableInstances, AllowAmbiguousTypes, FlexibleInstances, IncoherentInstances, FlexibleContexts #-}

module Effect.Nondet where

import Data.Free
import Effect.Void
import Typeclass.Coproduct
import Typeclass.TermAlgebra
import Typeclass.TermMonad
import Data.Identity
import Data.Codensity

data Nondet k where
    Or :: k -> k -> Nondet k

instance Functor Nondet where
    fmap f (Or x y) = Or (f x) (f y)

newtype NondetCarrier m a = NDC {unNDC :: m [a]}

instance Functor m => Functor (NondetCarrier m) where
    fmap f x =  NDC (fmap (fmap f) (unNDC x))

instance TermMonad m2 f => TermAlgebra (NondetCarrier m2) (Nondet + f) where
    con = NDC . (algNondet \/ con) . fmap unNDC
    var = NDC . genNondet

runNondet :: TermMonad m f => Codensity (NondetCarrier m) a -> m [a]
runNondet = unNDC . runCod var

genNondet :: TermMonad m f => a -> m [a]
genNondet x = var [x]

algNondet :: TermMonad m f => Nondet (m [a]) -> m [a]
algNondet (Or x y) =
            do  a <- x
                b <- y
                var (a ++ b)


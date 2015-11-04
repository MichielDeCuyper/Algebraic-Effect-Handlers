{-#LANGUAGE GADTSyntax, TypeOperators, MultiParamTypeClasses, UndecidableInstances,
AllowAmbiguousTypes, FlexibleInstances, IncoherentInstances, FlexibleContexts #-}

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

coin :: Free (Nondet + Void) Bool
coin = Con(Inl( Or (Var True) (Var False)))

genNondet :: TermMonad m g => a -> Identity (m [a])
genNondet x = Id (var [x])

algNondet :: TermMonad m g => Nondet (Identity (m [a])) -> Identity (m [a])
algNondet p = Id (algNondet' p)
    where 
        algNondet' (Or x y) =
            do  a <- runId x
                b <- runId y
                var (a ++ b)

handleNondet :: (TermAlgebra Identity g, TermMonad m g) => Free (Nondet + g) a -> Identity (m [a])
handleNondet = (fold (algNondet \/ con) genNondet)

------------------------------------------------------

newtype NondetCarrier m a = NDC {unNDC :: m [a]}

instance Functor m => Functor (NondetCarrier m) where
    fmap f x =  NDC (fmap (fmap f) (unNDC x))

instance TermMonad m2 f => TermAlgebra (NondetCarrier m2) (Nondet + f) where
    con = NDC . (algNondet' \/ con) . fmap unNDC
    var = NDC . genNondet'

genNondet' :: TermMonad m f => a -> m [a]
genNondet' x = var [x]

algNondet' :: TermMonad m f => (Nondet (m [a])) -> m [a]
algNondet' (Or x y) =
            do  a <- x
                b <- y
                var (a ++ b)

------------------------------------------------------
runNondet :: Functor g => Free (Nondet + g) a -> Free g [a]
runNondet = unNDC . handleNondetCod

handleNondetCod :: Functor g => Free (Nondet + g) a -> NondetCarrier (Free g) a
handleNondetCod = runCod var . handleNondetCod'

handleNondetCod' :: TermMonad m g => Free (Nondet + g) a -> Codensity (NondetCarrier m) a
handleNondetCod' = fold (algCod con) var

alg'' :: TermMonad m g => Nondet (NondetCarrier m a) -> NondetCarrier m a
alg'' p = NDC (algNondet' p)
    where 
        algNondet' (Or x y) =
            do  a <- unNDC $ x
                b <- unNDC $ y
                var (a ++ b)

gen'' :: TermMonad m f => a -> NondetCarrier m a
gen'' x = NDC (var [x])



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

-- newtype NondetCarrier m a = NDC {unNDC :: m [a]}

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



--handleNondetCod :: (TermAlgebra (NondetCarrier m) g, TermMonad m g) => Free (Nondet + g) a -> NondetCarrier m a
--handleNondetCod = runCod genNondet . handleNondetCod'

--handleNondetCod' :: (TermAlgebra (NondetCarrier m) g, TermMonad m g) => Free (Nondet + g) a -> Codensity (NondetCarrier m) a
--handleNondetCod' = fold (algCod (algNondet \/ con)) var
--handler :: (Functor f, Functor h) => Free f a -> h a
--handler = runCod genNondet . handler'

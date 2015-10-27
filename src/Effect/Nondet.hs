{-#LANGUAGE GADTSyntax, TypeOperators, MultiParamTypeClasses, UndecidableInstances,
AllowAmbiguousTypes, FlexibleInstances, IncoherentInstances #-}

module Effect.Nondet where

import Data.Free
import Typeclass.Coproduct
import Typeclass.TermAlgebra
import Typeclass.TermMonad
import Data.Identity

data Nondet k where
    Or :: k -> k -> Nondet k

instance Functor Nondet where
    fmap f (Or x y) = Or (f x) (f y)

coin :: Free Nondet Bool
coin = Con(Or (Var True) (Var False))

genNondet :: TermMonad m g => a -> m [a]
genNondet x = var [x]

algNondet :: TermMonad m g => Nondet (m [a]) -> m [a]
algNondet (Or x y) =
    do a <- x
       b <- y
       var (a ++ b)

handleNondet :: TermMonad m g => Free (Nondet + g) a -> m [a]
handleNondet = fold (algNondet \/ con) genNondet

--instance TermMonad m f => TermAlgebra m Nondet where
--    var = genNondet
--    con = algNondet


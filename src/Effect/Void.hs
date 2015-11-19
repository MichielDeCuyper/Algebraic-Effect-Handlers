{-#LANGUAGE MultiParamTypeClasses#-}

module Effect.Void (Void, runVoid) where

import Data.Free
import Data.Identity
import Data.Codensity
import Typeclass.TermMonad
import Typeclass.TermAlgebra

data Void k

instance Functor Void where
    fmap = undefined

runVoid :: Codensity Identity c -> c
runVoid = runId . runCod var

instance TermAlgebra Identity Void where
    var = Id
    con = undefined
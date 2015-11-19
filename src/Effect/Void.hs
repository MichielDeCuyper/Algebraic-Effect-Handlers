{-#LANGUAGE MultiParamTypeClasses#-}

module Effect.Void (Void, run) where

import Data.Free
import Data.Identity
import Data.Codensity
import Typeclass.TermMonad
import Typeclass.TermAlgebra

data Void k

instance Functor Void where
    fmap = undefined

run :: Codensity Identity c -> c
run = runId . runCod var

instance TermAlgebra Identity Void where
    var = Id
    con = undefined
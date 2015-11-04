{-#LANGUAGE MultiParamTypeClasses#-}

module Effect.Void (Void, handleVoid, runVoid) where

import Data.Free
import Data.Identity
import Typeclass.TermMonad
import Typeclass.TermAlgebra

data Void k

instance Functor Void where
    fmap = undefined

runVoid :: Free Void a -> a
runVoid = runId . handleVoid

handleVoid :: Free Void a -> Identity a
handleVoid = fold undefined Id

instance TermAlgebra Identity Void where
    var = Id
    con = undefined

instance TermMonad Identity Void where
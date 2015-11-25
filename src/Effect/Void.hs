{-#LANGUAGE MultiParamTypeClasses#-}

module Effect.Void (Void, run) where

import Data.Codensity
import Typeclass.TermMonad
import Typeclass.TermAlgebra

data Void k

instance Functor Void where
    fmap = undefined

newtype VoidCarrier a = VC {unVC :: a}

instance Functor VoidCarrier where
    fmap f x =  VC (f (unVC x))

run :: Codensity VoidCarrier c -> c
run = unVC . runCod var

instance TermAlgebra VoidCarrier Void where
    var = VC
    con = undefined
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE GADTSyntax #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE UndecidableInstances #-}

{-|
Module: Effect.Error
Description: Error Effect Handler
Copyright: (c) Michiel De Cuyper 2015
License: MIT
Maintainer: Michiel.DeCuyper@student.kuleuven.be
Stability: Experimental
-}

-- Carriers van vorm s -> m (f a)

module Effect.Error (

    -- * Datatype
    Error (Throw),

    -- * Handlers
    runError,

    -- * Smart constructors for operators
    throw

    ) where

import Data.Codensity
import Typeclass.Coproduct
import Typeclass.TermMonad
import Typeclass.TermAlgebra

data Error e k where
    Throw :: e -> Error e k

instance Functor (Error e) where
    fmap f (Throw e) = Throw e

newtype ErrorCarrier e m a = EC {unEC :: m (Either e a)}

instance Functor m => Functor (ErrorCarrier e m) where
    fmap f x = EC (fmap (fmap f) (unEC x)) 

instance TermMonad m f => TermAlgebra (ErrorCarrier e m) (Error e + f) where
    con = EC . (algError \/ con) . fmap unEC
    var = EC . genError

throw :: (TermAlgebra h f, Error e :< f) => e -> h a
throw e = inject (Throw e)

genError :: TermMonad m f => a -> m (Either e a)
genError x = var (Right x)

algError :: TermMonad m f => Error e (m (Either e a)) -> m (Either e a)
algError (Throw e) = var (Left e)

runError :: TermMonad m f => Codensity (ErrorCarrier e m) a -> m (Either e a)
runError = unEC . runCod var



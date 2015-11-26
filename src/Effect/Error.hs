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
    fmap _ (Throw e) = Throw e

newtype ErrorCarrier e m a = EC {unEC :: Either e (m a)}

instance Functor m => Functor (ErrorCarrier e m) where
    fmap f x = EC (fmap (fmap f) (unEC x)) 

instance (TermMonad m f, TermAlgebra (Either e) f) => TermAlgebra (ErrorCarrier e m) (Error e + f) where
    con = EC . (algError \/ con) . fmap unEC
    var = EC . genError

throw :: (TermAlgebra h f, Error e :< f) => e -> h a
throw e = inject (Throw e)

genError :: Monad m => a -> Either e (m a)
genError x = Right (return x)

algError :: TermMonad m f => Error e (Either e (m a)) -> Either e (m a)
algError (Throw k) = Left k

runError :: (TermMonad m f, TermAlgebra (Either e) f) => Codensity (ErrorCarrier e m) a -> Either e (m a)
runError = unEC . runCod var
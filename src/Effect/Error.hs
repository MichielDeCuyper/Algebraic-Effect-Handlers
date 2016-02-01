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
    catch,

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

newtype ErrorCarrier e m a = EC {unEC :: m (Either e a)}

instance Functor m => Functor (ErrorCarrier e m) where
    fmap f x = EC (fmap (fmap f) (unEC x))

instance TermMonad m f => TermAlgebra (ErrorCarrier e m) (Error e + f) where
    con = EC . (algError \/ con) . fmap unEC
    var = EC . genError

throw :: (TermMonad m f, Error e :< f) => e -> m a
throw e = inject (Throw e)

genError :: TermMonad m f => a -> m (Either e a)
genError x = var (Right x)

algError :: TermMonad m f => Error e (m (Either e a)) -> m (Either e a)
algError (Throw e) = var (Left e)

runError :: TermMonad m f => Codensity (ErrorCarrier e m) a -> m (Either e a)
runError = unEC . runCod var

-- TODO Write catch handlerl
newtype CatchCarrier m e a = CC {unCC :: (e -> m a) -> m a}

instance Functor m => Functor (CatchCarrier m e) where
  fmap f x = undefined -- TODO Overal a in b veranderen, blijkt niet zo simpel

algCatch :: TermMonad m (Error e + f) => Error e ((e -> m a) -> m a) -> (e -> m a) -> m a
algCatch (Throw e) h = h e

genCatch :: TermMonad m f => a -> (e -> m a) -> m a
genCatch a = const (return a)

conCatch :: (Functor f, Functor g, TermAlgebra m (g + f)) => f ((e -> m a) -> m a) -> (e -> m a) -> m a
conCatch a e = con (Inr (fmap (\g -> g e) a))

instance (Functor f, TermMonad m (Error e +f)) => TermAlgebra (CatchCarrier m e) (Error e + f) where
  var = CC . genCatch -- TODO Is Carrier wel correct??
  con = CC . (algCatch \/ conCatch) . fmap unCC -- TODO

catch :: (Functor f, TermMonad m (Error e + f)) => Codensity (CatchCarrier m e) a -> (e -> m a) -> m a
catch = unCC . runCod var

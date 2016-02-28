{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE GADTSyntax #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE UndecidableInstances #-}
{-#LANGUAGE DeriveFunctor #-}

{-|
Module: Effect.Writer
Description: Writer Effect Handler
Copyright: (c) Michiel De Cuyper 2015
License: MIT
Maintainer: Michiel.DeCuyper@student.kuleuven.be
Stability: Experimental
-}
module Effect.Writer (

    -- * Datatype
    Writer (Tell),

    -- * Handlers
    runWriter,
    listen,

    -- * Smart constructors for operators
    tell

    ) where

import Data.Codensity
import Typeclass.Coproduct
import Typeclass.TermMonad
import Typeclass.TermAlgebra

-- | The Writer datatype.
--
data Writer w k where
    Tell :: w -> k -> Writer w k

-- | Smart constructor for the 'Tell' operation
-- Use this one instead of the Tell-constructor of the datatype!
--
-- Constructs a 'Tell' operation given a monoidical value __w__ and a continuation __k__
--
-- Usage: @tell w k@
--
-- = Example
--
-- @let x = tell "abc" :: TermAlgebra h (Writer String + Void) => h ()@
--
--  >>> run . runWriter $ x
-- ("abc", ())
--
-- @let y = tell "xyz" x :: TermAlgebra h (Writer String + Void) => h Int@
--
-- >>> run . runWriter $ y
-- ("xyzabc", 123)
tell :: (TermMonad m f, Writer w :< f) => w -> m ()
tell w = inject (Tell w (var ()))

instance Functor (Writer w) where
    fmap f (Tell w k) = Tell w (f k)

newtype WriterCarrier m w a = WC {unWC :: m (a, w)}

instance Functor m => Functor (WriterCarrier m w) where
    fmap f x = WC (fmap (\(a, w) -> (f a , w)) (unWC x))

instance (Monoid w, TermMonad m f) => TermAlgebra (WriterCarrier m w) (Writer w + f) where
    con = WC . (algWriter \/ con) . fmap unWC
    var = WC . genWriter

-- | Handler for the Writer effect.
-- Interprets __Tell__ operations
runWriter :: (Monoid w, TermMonad m f) => Codensity (WriterCarrier m w) a -> m (a, w)
runWriter = unWC . runCod var

genWriter :: (TermMonad m f, Monoid w) => a -> m (a, w)
genWriter x = return (x, mempty)

algWriter :: (TermMonad m f, Monoid w) => Writer w (m (a,w)) -> m (a, w)
algWriter (Tell w k) = k >>= f
  where f (x, w') = return (x, w `mappend` w')

newtype ListenCarrier m w a = LC {unLC :: m (a, w)} deriving Functor

instance (Functor f, Monoid w, TermMonad m (Writer w + f)) => TermAlgebra (ListenCarrier m w) (Writer w + f) where
  var = LC . genListen
  con = LC . (algListen \/ conListen) . fmap unLC

genListen :: (TermMonad m f, Monoid w) => a -> m (a, w)
genListen x = return (x, mempty)

algListen :: (Functor f, Monoid w, TermMonad m (Writer w + f)) => Writer w (m (a, w)) -> m (a, w)
algListen (Tell w k) = con(Inl(Tell w (k >>= f)))
  where f (a, w') = return (a, w `mappend` w')

conListen :: (Functor f, Functor g, TermAlgebra m (g + f)) => f (m (a, w)) -> m (a, w)
conListen a = con(Inr a)

listen :: (Monoid w, Functor f, TermMonad m (Writer w + f)) => Codensity (ListenCarrier m w) a -> m (a, w)
listen = unLC . runCod var
------ This works! Order of effects matters!
-- First handles Writer, then Nondet
--ex1 :: TermAlgebra h (Writer String + (Nondet + Void)) => h Bool
--ex1 = tell "abc" coin

---- First handles Nondet, then Writer
--ex2 :: TermAlgebra h (Nondet + (Writer String + Void)) => h Bool
--ex2 = tell "abc" coin

{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE GADTSyntax #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE UndecidableInstances #-}

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
-- @let x = tell "abc" (var 123) :: TermAlgebra h (Writer String + Void) => h Int@
--
--  >>> run . runWriter $ x
-- ("abc", 123)
--
-- @let y = tell "xyz" x :: TermAlgebra h (Writer String + Void) => h Int@
--
-- >>> run . runWriter $ y
-- ("xyzabc", 123)
tell :: (TermMonad m f, Writer w :< f) => w -> m ()
tell w = inject (Tell w (var ()))

instance Functor (Writer w) where
    fmap f (Tell w k) = Tell w (f k)

newtype WriterCarrier m w a = WC {unWC :: m (w, a)}

instance Functor m => Functor (WriterCarrier m w) where
    fmap f x = WC (fmap (fmap f) (unWC x))

instance (Monoid w, TermMonad m f) => TermAlgebra (WriterCarrier m w) (Writer w + f) where
    con = WC . (algWriter \/ con) . fmap unWC
    var = WC . genWriter

-- | Handler for the Writer effect.
-- Interprets __Tell__ operations
runWriter :: (Monoid w, TermMonad m f) => Codensity (WriterCarrier m w) a -> m (w, a)
runWriter = unWC . runCod var

genWriter :: (TermMonad m f, Monoid w) => a -> m (w, a)
genWriter x = return (mempty, x)

algWriter :: (TermMonad m f, Monoid w) => Writer w (m (w,a)) -> m (w, a)
algWriter (Tell w k) = k >>= (\(w', x) -> return (w `mappend` w', x))

------ This works! Order of effects matters!
-- First handles Writer, then Nondet
--ex1 :: TermAlgebra h (Writer String + (Nondet + Void)) => h Bool
--ex1 = tell "abc" coin

---- First handles Nondet, then Writer
--ex2 :: TermAlgebra h (Nondet + (Writer String + Void)) => h Bool
--ex2 = tell "abc" coin

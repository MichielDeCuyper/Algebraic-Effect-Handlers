{-#LANGUAGE TypeOperators, GADTSyntax, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, IncoherentInstances#-}

module Effect.Reader where

import Data.Codensity
import Typeclass.Coproduct
import Typeclass.TermMonad
import Typeclass.TermAlgebra

data Reader e k where
    Ask :: (e -> e) -> k -> Reader e k

instance Functor (Reader e) where
    fmap f (Ask e k) = Ask e (f k) 

newtype ReaderCarrier m e a = RC {unRC :: e -> m a}

instance Functor m => Functor (ReaderCarrier m e) where
    fmap f x = RC (fmap (fmap f) (unRC x))

instance TermMonad m f => TermAlgebra (ReaderCarrier m e) (Reader e + f) where
    var = RC . genReader
    con = RC . (algReader \/ conReader) . fmap unRC

genReader :: TermMonad m f => a -> e -> m a
genReader x = const (var x)

algReader :: TermMonad m f => Reader e (e -> m a) -> e -> m a
algReader (Ask f k) e = k e

runReader :: TermMonad m f => Codensity (ReaderCarrier m e) a -> e -> m a
runReader = unRC . runCod var 

conReader :: (Functor f, TermAlgebra m f) => f (e -> m a) -> e -> m a
conReader op s = con (fmap (\m -> m s) op)
{-#LANGUAGE TypeOperators, GADTSyntax, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances#-}

module Effect.Reader where

import Data.Codensity
import Typeclass.Coproduct
import Typeclass.TermMonad
import Typeclass.TermAlgebra

data Reader e k where
    Ask :: (e -> k) -> Reader e k

ask :: (TermMonad m f, Reader e :< f) => m e
ask = inject (Ask var)

instance Functor (Reader e) where
    fmap f (Ask g) = Ask (f . g)

newtype ReaderCarrier m e a = RC {unRC :: e -> m a}

instance Functor m => Functor (ReaderCarrier m e) where
    fmap f x = RC (fmap (fmap f) (unRC x))

instance TermMonad m f => TermAlgebra (ReaderCarrier m e) (Reader e + f) where
    var = RC . genReader
    con = RC . (algReader \/ conReader) . fmap unRC

genReader :: TermMonad m f => a -> e -> m a
genReader x = const (var x)

algReader :: TermMonad m f => Reader e (e -> m a) -> e -> m a
algReader (Ask g) e = g e e

runReader :: TermMonad m f => Codensity (ReaderCarrier m e) a -> e -> m a
runReader = unRC . runCod var

conReader :: (Functor f, TermAlgebra m f) => f (e -> m a) -> e -> m a
conReader op s = con (fmap (\m -> m s) op)

newtype LocalCarrier m e a = LoC {unLoC :: (e -> e) -> m a}

instance Functor m => Functor (LocalCarrier m e) where
  fmap f a = LoC (fmap (fmap f) (unLoC a))

instance (Functor f, TermMonad m (Reader e +f)) => TermAlgebra (LocalCarrier m e) (Reader e + f) where
  var = LoC . genLocal
  con = LoC . (algLocal \/ conLocal) . fmap unLoC

genLocal :: TermMonad m f => a -> (e -> e) -> m a
genLocal x = const (return x)

algLocal :: (Functor f, TermMonad m (Reader e + f)) => Reader e ((e -> e) -> m a) -> (e -> e) -> m a
algLocal (Ask k) g = con(Inl(Ask(\e -> k (g e) g)))

conLocal :: (Functor f, Functor g, TermAlgebra m (g + f)) => f ((e -> e) -> m a) -> (e -> e) -> m a
conLocal a e = con (Inr (fmap (\g -> g e) a))

local :: (Functor f, TermMonad m (Reader e + f)) => Codensity (LocalCarrier m e) a -> (e -> e) -> m a
local = unLoC . runCod var

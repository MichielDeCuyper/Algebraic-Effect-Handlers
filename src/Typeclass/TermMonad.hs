{-#LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
UndecidableInstances#-}

module Typeclass.TermMonad (TermMonad) where

import Typeclass.TermAlgebra
import Data.Free

class (Monad m, TermAlgebra m f) => TermMonad m f | m -> f where

instance {-# OVERLAPPING #-} (Monad m, TermAlgebra m f) => TermMonad m f where

instance {-# OVERLAPPING #-} Functor f => TermMonad (Free f) f where
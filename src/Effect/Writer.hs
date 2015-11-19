{-#LANGUAGE TypeOperators, GADTSyntax, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, IncoherentInstances#-}

module Effect.Writer where

import Data.Codensity
import Effect.Void
import Effect.Nondet
import Typeclass.Coproduct
import Typeclass.TermMonad
import Typeclass.TermAlgebra

data Writer w k where
    Tell :: w -> k -> Writer w k

tell :: (TermAlgebra h f, Writer w :< f) => w -> h a -> h a
tell w k = inject (Tell w k)

instance Functor (Writer w) where
    fmap f (Tell w k) = Tell w (f k) 

newtype WriterCarrier m w a = WC {unWC :: m (w, a)}

instance Functor m => Functor (WriterCarrier m w) where
    fmap f x = WC (fmap (fmap f) (unWC x)) 

instance (Monoid w, TermMonad m f) => TermAlgebra (WriterCarrier m w) (Writer w + f) where
    con = WC . (algWriter \/ con) . fmap unWC
    var = WC . genWriter

runWriter :: (Monoid w, TermMonad m f) => Codensity (WriterCarrier m w) a -> m (w, a)
runWriter = unWC . runCod var

genWriter :: (Monad m, Monoid w) => a -> m (w, a)
genWriter x = return (mempty, x)

algWriter :: (Monad m, Monoid w) => Writer w (m (w,a)) -> m (w, a)
algWriter (Tell w k) = k >>= (\(w', x) -> return (w `mappend` w', x))

------ This works! Order of effects matters!
-- First handles Writer, then Nondet
ex1 :: TermAlgebra h (Writer String + (Nondet + Void)) => h Bool
ex1 = tell "abc" coin

-- First handles Nondet, then Writer
ex2 :: TermAlgebra h (Nondet + (Writer String + Void)) => h Bool
ex2 = tell "abc" coin


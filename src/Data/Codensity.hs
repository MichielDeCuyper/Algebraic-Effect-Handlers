{-#LANGUAGE RankNTypes, DeriveFunctor, FlexibleInstances, UndecidableInstances, 
MultiParamTypeClasses#-}

module Data.Codensity(Codensity, runCod)where

import Typeclass.TermAlgebra
import Typeclass.TermMonad

newtype Codensity h a = Codensity {unCod :: forall x . (a -> h x) -> h x}
    deriving (Functor)

instance Applicative (Codensity h) where
    pure = return
    mf <*> mx = do
        f <- mf
        x <- mx
        return (f x)

instance Monad (Codensity h) where
    return x = Codensity (\k -> k x)
    Codensity m >>= f = Codensity (\k -> m (\a -> unCod (f a) k)) 

runCod :: (a -> f x) -> Codensity f a -> f x
runCod g m = unCod m g

instance TermAlgebra h f => TermAlgebra (Codensity h) f where
    var = return
    con = algCod con

algCod :: Functor f => (forall x . f (h x) -> h x) -> (f (Codensity h a) -> Codensity h a)
algCod alg op = Codensity (\k -> alg (fmap (\m -> unCod m k) op))

instance TermAlgebra h f => TermMonad (Codensity h) f where




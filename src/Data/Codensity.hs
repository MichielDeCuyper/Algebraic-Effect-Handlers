{-#LANGUAGE RankNTypes, DeriveFunctor#-}

module Data.Codensity(Codensity, runCod)where


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
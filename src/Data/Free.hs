{-# LANGUAGE GADTSyntax #-}
module Data.Free (Free(Var, Con), fold) where

data Free f a where
    Var :: a -> Free f a
    Con :: f (Free f a) -> Free f a

instance Functor f => Functor (Free f) where
    fmap f (Var x) = Var (f x)
    fmap f (Con op) = Con $ fmap (fmap f) op 

instance Functor f => Applicative (Free f) where
     pure = return
     Var f <*> s = fmap f s
     Con faf <*> as = Con (fmap (<*> as) faf)

instance Functor f =>  Monad (Free f) where
    return x = Var x
    m >>= f = fold Con f m

fold :: Functor f => (f b -> b) -> (a -> b) -> (Free f a -> b)
fold alg gen (Var x) = gen x
fold alg gen (Con op) = alg (fmap (fold alg gen) op)

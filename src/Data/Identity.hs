module Data.Identity where

newtype Identity a = Id {runId :: a}

instance Show a => Show (Identity a) where
    show (Id x) = "id: " ++ show x-- for debug purposes

instance Functor Identity where
    fmap f (Id a) = Id (f a) 

instance Applicative Identity where
    pure = Id
    Id f <*> Id x = Id (f x)  

instance Monad Identity where
    return = Id
    m >>= f = f (runId m)
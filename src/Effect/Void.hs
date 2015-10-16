module Effect.Void (Void, handleVoid) where

import Data.Free

data Void k

instance Functor Void where
    fmap = undefined

handleVoid :: Free Void a -> a
handleVoid = fold undefined id
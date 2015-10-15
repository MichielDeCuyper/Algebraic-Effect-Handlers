{-# LANGUAGE GADTSyntax, RankNTypes, UndecidableInstances,
FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeOperators #-}

data Void k

instance Functor Void  where
    fmap _ _= undefined

data State s k where
    Put :: s -> k -> State s k
    Get :: (s -> k) -> State s k

instance Functor (State s) where
    fmap f (Put s k) = Put s (f k) 
    fmap f (Get k) = Get (f . k)

data Free f a where
    Var :: a -> Free f a
    Con :: (f (Free f a)) -> Free f a

instance Functor f => Functor (Free f) where
    fmap f (Var x) = Var (f x)
    fmap f (Con op) = Con $ fmap (fmap f) op 

instance Functor f =>  Monad (Free f) where
    return x = Var x
    m >>= f = fold Con f m

instance Functor f => Applicative (Free f) where
     pure = return
     Var f <*> s = fmap f s
     Con faf <*> as = Con (fmap (<*> as) faf)

data Nondet k where
    Or :: k -> k -> Nondet k

instance Functor Nondet where
    fmap f (Or x y) = Or (f x) (f y) 

coin :: Free Nondet Bool
coin = Con ( Or (Var True) (Var False))

fold :: Functor f => (f b -> b) -> (a -> b) -> (Free f a -> b)
fold alg gen (Var x) = gen x
fold alg gen (Con op) = alg (fmap (fold alg gen) op)

handleVoid :: Free Void a -> a
handleVoid = fold undefined id

instance Functor m => Functor (LogStateCarrier s m) where
    fmap f x =  LSC (fmap (fmap f) (unLSC x))

class Functor f => TermAlgebra h f | h -> f where
    var :: forall a . a -> h a
    con :: forall a . f (h a) -> h a

instance Functor f => TermAlgebra (Free f) f where
    var = Var
    con = Con

class (Monad m, TermAlgebra m f) => TermMonad m f | m -> f

instance (Monad m, TermAlgebra m f) => TermMonad m f

instance Functor f => TermMonad (Free f) f

newtype LogStateCarrier s m a = LSC {unLSC :: s -> m a}

instance (TermMonad m (Writer String + Void)) => TermAlgebra (LogStateCarrier s m) (State s) where
    var = LSC . genState'
    con = LSC . algLogState . fmap unLSC


data (+) f g a where
    Inl :: f a -> (f + g) a
    Inr :: g a -> (f + g) a

instance (Functor f, Functor g) => Functor (f + g) where
    fmap f (Inl s) = Inl (fmap f s) 
    fmap f (Inr s) = Inr (fmap f s)

data Writer w k where
    Tell :: w -> k -> Writer w k

instance Functor (Writer w) where
    fmap f (Tell w k) = Tell w (f k) 

algLogState :: (TermMonad m (Writer String + Void)) 
            => State s (s -> m a) -> s -> m a
algLogState (Get k) s = k s s
algLogState (Put s' k) _ = con (Inl (Tell "put" (k s')))

genState' :: TermMonad m f => a -> (s -> m a)
genState' x = const (var x)

data Id x where
    Id :: x -> Id x

instance Functor Id where
    fmap f (Id x) = Id $ f x




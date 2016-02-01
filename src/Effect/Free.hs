{-#LANGUAGE TypeOperators, GADTSyntax, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, IncoherentInstances#-}

import Data.Char
import Data.Codensity
import Typeclass.TermAlgebra
import Typeclass.TermMonad
import Typeclass.Coproduct

data Free f a where
  Var :: a -> Free f a
  Con  :: f (Free f a) -> Free f a

data Reader e a where
  Ask :: (e -> a) -> Reader e a

instance Functor (Reader e) where
  fmap f (Ask g) = Ask (f . g)

main :: IO ()
main = putStr "Hello world"

ex = Con (Ask (\e -> Var("Hello " ++ e)))

local' :: (e -> e) -> Free (Reader e) a -> Free (Reader e) a
local' f (Var x) = Var x
local' f (Con (Ask k)) = Con (Ask (local' f . k . f))

newtype LocalCarrier m e a = LoC {unLoC :: (e -> e) -> m a}

instance Functor m => Functor (LocalCarrier m e) where
  fmap f a = LoC (fmap (fmap f) (unLoC a))

instance (Functor f, TermMonad m (Reader e +f)) => TermAlgebra (LocalCarrier m e) (Reader e + f) where
  var = LoC . genLocal
  con = LoC . (algLocal \/ conLocal) . fmap unLoC

genLocal :: TermMonad m f => a -> (e -> e) -> m a
genLocal x = const (return x)

algLocal :: (Functor f, TermMonad m (Reader e + f)) => Reader e ((e -> e) -> m a) -> (e -> e) -> m a
algLocal (Ask k) g = con(Inl(Ask(\e -> k e g)))

conLocal :: (Functor f, Functor g, TermAlgebra m (g + f)) => f ((e -> e) -> m a) -> (e -> e) -> m a
conLocal a e = con (Inr (fmap (\g -> g e) a))

local :: (Functor f, TermMonad m (Reader e + f)) => Codensity (LocalCarrier m e) a -> (e -> e) -> m a
local = unLoC . runCod var

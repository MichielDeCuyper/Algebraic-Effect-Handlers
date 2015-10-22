# Algebraic-Effect-Handlers
Library for Algebraic Effect Handlers in Haskell, as part of my Bachelor's thesis.

Based on *Fusion For Free* by prof. Tom Schrijvers.

## The problem
Haskell functions are pure and thereby yield no side effects. 
A function `Int -> Int -> Int` won't format your disk, as it has only works for Integers 
(compared to e.g. a Java function `public int foo(int a, int b) {...}`, where you have no clue what happens on the inside).

Yet, sometimes we do need side-effects:
requesting an item out of a database may return an empty value, asking the user for input, the entered password might be incorrect and you need to throw an error.
In Haskell, these side-effects are captured in **monads** (respectively: `Maybe`, `IO` and `Either`).

One monad can only respond to one type of side-effects. 
The IO monad can't both control user input and respond to errors, for example.
While the composition of two functors itself is a functor and the composition of two applicative functors itself is also an applicative,
the same can't be said for monads (in general).

```haskell
-- Codebit 1: Functor composition yields a functor
newtype (f . g) a = f (g a)
instance (Functor f, Functor g) => Functor (f . g) where
  fmap f a = fmap (fmap f) a
```
The problem with monad composition is the following:

```haskell
-- Codebit 2: Monad composition does not yield a monad
instance (Monad f, Monag g) => Monad (f . g) where
  return = pure -- Derivable from applicative
  join :: f (g (f (g a))) -> f (g a)
  join x = ... -- Not possible. We have something for f (f a) -> f a and g (g a) -> g a but not something for an alternating sequence
```

## A First Approach

## Fusion
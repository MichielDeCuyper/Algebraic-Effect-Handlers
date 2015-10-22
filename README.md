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
While the composition of two functors itself is a functor, the same can't be said for monads (in general).

```haskell
-- Codebit 1: Functor composition yields another functor
newtype (f . g) a = f (g a)
instance (Functor f, Functor g) => Functor (f . g) where
  fmap f a = fmap (fmap f) a
```

## A First Approach

## Fusion
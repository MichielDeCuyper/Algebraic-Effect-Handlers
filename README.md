# Algebraic-Effect-Handlers
Library for Algebraic Effect Handlers in Haskell, as part of my Bachelor's thesis.

Based on *Fusion For Free* by prof. Tom Schrijvers.

## The problem
Haskell functions are pure and therefor yield no side effects, unless we want them to.
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
  join x = undefined -- Not possible. We have something for f (f a) -> f a and g (g a) -> g a
               -- but not something for an alternating sequence
```

The standard approach is the one of **monad transformers**,
where we redefine a monad inside a newtype with an extension to plug a new monad in.

```haskell
-- Codebit 3: The StateT monad transformer
newtype StateT s m a = St {runSt :: s -> m (s,a)} -- Big difference being the m in the runSt function
lift m = St (\s ->
  do a <- m
     return (a, s)

instance Monad m => Monad (State s m) where
  return x = lift (return x)
  (>>=) m f = .. -- TODO

get :: StateT s m s
get = St (\s -> return (s,s))

put :: StateT s m ()
put s' = St (\s -> return ((), s'))
```

Every monad we want to extend requires this work (defining the newtype, the lift function, the monad instance,...).
While there are libraries that do this work for you, lifting functions all the time isn't that efficient to do.

This library offers another approach, that of **algebraic effect handlers**, which offer easier composability.
The goal of this library is to be user-friendly, complete and efficient.
### User-Friendly
This library aims to be user-friendliness by offering smart constructors and easy-composable handlers.
What happens behind the scenes will make use of complex typeclasses and functions,
but the API exposed to the user will be as easy as possible.
### Complete
This library aims to be complete by offering the same functionality offered by standard Monad Transformers,
but even going further by exploring more exotic effects.

### Efficient
The goal of this library is to match the efficiency of Monad Transformers.
It has no use to aim for slower.
The efficiency is achieved by *handler fusion*, described in *Fusion For Free* by T. Schrijvers (2015)

## A Gentle Introduction

## A First Approach

## Fusion

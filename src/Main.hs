{-#LANGUAGE FlexibleContexts, TypeOperators#-}

module Main where

import Data.Free
import Effect.State
import Effect.Void
import Effect.Nondet
import Effect.Writer
import Effect.LogState
import Typeclass.TermAlgebra
import Typeclass.TermMonad
import Typeclass.Coproduct

main :: IO ()
main = putStrLn "Hello World"

--program :: Int -> Free (State Int) Int
program n
    | n <= 0 = con (Inl(Get var))
    | otherwise = con (Inl(Get (\s -> con(Inl(Put (s + n) (program (n-1)))))))

program' n
    | n <= 0 = con (Get var)
    | otherwise = con (Get (\s -> con(Put (s + n) (program' (n-1)))))

program_ n
    | n <= 0 = get var
    | otherwise = get (\s -> put (s+n) program_ (n-1))

test _ = tell "abc" (or' (var True) (var False))
----coin :: Free (Nondet + Void) Bool
coin _ = con(Inl( Or (var True) (var False)))

example n = runVoid $ (runState $ program 1) n

or' :: (TermAlgebra h f, Nondet :< f) => h a -> h a -> h a
or' p q = inject (Or p q)

get :: (TermAlgebra h f, State s :< f) => (s -> h a) -> h a
get k = inject (Get k)

put :: (TermAlgebra h f, State s :< f) => s -> h a -> h a
put s k = inject (Put s k)

tell :: (TermAlgebra h f, Writer w :< f) => w -> h a -> h a
tell w k = inject (Tell w k)


coin' :: (TermAlgebra h f, Nondet :< f) => h Bool
coin' = or' (var True) (var False)
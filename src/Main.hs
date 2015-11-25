{-#LANGUAGE FlexibleContexts, TypeOperators, AllowAmbiguousTypes#-}

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
main = putStrLn "Algebraic Effect Handlers"

program n
    | n <= 0 = con (Inl(Get var))
    | otherwise = con (Inl(Get (\s -> con(Inl(Put (s + n) (program (n-1)))))))

program' n
    | n <= 0 = con (Get var)
    | otherwise = con (Get (\s -> con(Put (s + n) (program' (n-1)))))

program_ :: TermAlgebra h (State Int + Void) => Int -> h Int
program_ n
    | n <= 0 = get var
    | otherwise = get (\a -> (put (a+n) (program_ (n-1))))

example n = run $ (runState $ program 0) n







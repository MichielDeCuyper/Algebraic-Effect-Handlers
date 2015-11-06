{-#LANGUAGE FlexibleContexts#-}

module Main where

import Data.Free
import Effect.State
import Effect.Void
import Effect.Nondet
import Effect.LogState
import Typeclass.TermMonad
import Typeclass.Coproduct

main :: IO ()
main = putStrLn "Hello World"

--program :: Int -> Free (State Int) Int
program n
    | n <= 0 = con (Inl(Get var))
    | otherwise = con (Inl(Get (\s -> con(Inl(Put (s + n) (program (n-1)))))))


----coin :: Free (Nondet + Void) Bool
coin _ = con(Inl( Or (var True) (var False)))

example n = runVoid $ (runState $ program 1) n
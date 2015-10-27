module Main where

import Data.Free
import Effect.State
import Effect.Void
import Effect.Nondet
import Data.Nondet
import Typeclass.TermMonad

main :: IO ()
main = putStrLn "Hello World"

program :: Int -> Free (State Int) Int
program n
    | n <= 0 = Con (Get var)
    | otherwise = Con (Get (\s -> Con(Put (s + n) (program (n-1)))))

--example :: Int -> (String, Int)
--example n = (handleVoid . handleState (program 0)) n

example :: Int -> Int -> Int
example n s = handleVoid $ help (program n) s
    where help = fold algState genState
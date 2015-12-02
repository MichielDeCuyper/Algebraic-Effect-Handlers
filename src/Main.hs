{-#LANGUAGE FlexibleContexts, TypeOperators, AllowAmbiguousTypes#-}

module Main where

-- Datatypes
import Data.Codensity
-- Effects
import Effect.Error
import Effect.LogState
import Effect.Nondet
import Effect.Reader
import Effect.State
import Effect.Void
import Effect.Writer
-- Typeclasses
import Typeclass.Coproduct
import Typeclass.TermAlgebra
import Typeclass.TermMonad

main :: IO ()
main = print $ show f
    where f = run $ runState count 100000000

ex :: TermAlgebra h (Error String + Void) => Int -> h Int
ex n
    | n <= 0 = throw "Invalid input"
    | otherwise = var n

exa :: TermMonad h (State Int + Writer String + Void) => Int -> h Int
exa n
    | n <= 0 = get
    | otherwise = do a <- get 
                     put (a+n)
                     tell ("put value: " ++ show a ++ "+" ++ show n ++ " = " ++ show (a+n) ++ ". \n")
                     exa (n-1)

count :: TermMonad h (State Int + Void) => h Int
count = do i <- get
           if i == 0 then return i
           else do put (i - 1)
                   count

greeter :: TermMonad m (Reader String + Void) => m String
greeter = do name <- ask
             return ("Hello " ++ name)
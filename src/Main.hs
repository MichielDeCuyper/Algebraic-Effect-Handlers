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
main = putStrLn "Algebraic Effect Handlers"


ex :: TermAlgebra h (Error String + Void) => Int -> h Int
ex n
    | n <= 0 = throw "Invalid input"
    | otherwise = var n

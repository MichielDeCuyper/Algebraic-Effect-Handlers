{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

-- Datatypes
-- Effects
import           Effect.Writer
import Effect.Reader
--import Effect.RWS
import           Effect.LogState
import           Effect.State
import           Effect.Void
import Effect.Error
--import Effect.Writer
-- Typeclasses
import           Typeclass.Coproduct
import           Typeclass.TermMonad

main :: IO ()
main = putStrLn "Hello"

example :: TermMonad h (State Int + Void) => Int -> h Int
example n
  | n <= 0 = get
  | otherwise =
      do a <- get
         put (a + n)
         example (n - 1)

a :: (String, Int)
a = run . runWriter $ runLogState (example 1) 3

b :: Int
b = run $ runState (example 1) 4

-- main :: IO ()
-- main = print $ show f
--    where f = run $ runState count 100000000

--ex :: TermAlgebra h (Error String + Void) => Int -> h Int
--ex n
--    | n <= 0 = throw "Invalid input"
--    | otherwise = var n

--exa :: TermMonad h (State Int + Writer String + Void) => Int -> h Int
--exa n
--    | n <= 0 = get
--    | otherwise = do a <- get
--                     put (a+n)
--                     tell ("put value: " ++ show a ++ "+" ++ show n ++ " = " ++ show (a+n) ++ ". ")
--                     exa (n-1)

-- test :: TermMonad h (RWS String Int String + Void) => Int -> h Int
-- test n
--       | n <= 0 = get
--       | otherwise = do a <- get
--                        put (a+n)
--                        tell ("put value: " ++ show a ++ "+" ++ show n ++ " = " ++ show (a+n) ++ ". ")
--                        test (n-1)

--count :: TermMonad h (State Int + Void) => h Int
--count = do i <- get
--           if i == 0 then return i
--           else do put (i - 1)
--                   count

greeter :: TermMonad m (Reader String + Void) => m String
greeter = do name <- ask
             return ("Hello " ++ name)


fault :: TermMonad m (Error String + Void) => m a
fault = throw "Derp"

{-#LANGUAGE TypeOperators#-}
{-#LANGUAGE FlexibleContexts#-}

module Benchmarks(
  benchHandler1,
  benchTrans1,
  benchHandler2,
  benchTrans2
) where

import Typeclass.TermMonad
import Typeclass.Coproduct

import Effect.State
import Effect.Writer
import Effect.Void

import qualified Control.Monad.State as MTL
import qualified Control.Monad.Writer as MTL

benchmark1h :: TermMonad m (State Int + Void) => Int -> m Int
benchmark1h n
  | n <= 0 = get
  | otherwise = do
      a <- get
      put (a + n)
      benchmark1h (n-1)

benchmark1m ::  Int -> MTL.State Int Int
benchmark1m n
    | n <= 0 = MTL.get
    | otherwise = do
        a <- MTL.get
        MTL.put (a + n)
        benchmark1m (n - 1)

benchHandler1 :: Int -> (Int, Int)
benchHandler1 n = run $ runState (benchmark1h n) 0

benchTrans1 :: Int -> (Int,Int)
benchTrans1 n = MTL.runState (benchmark1m n) 0

benchmark2h :: TermMonad m (State Int + Writer String + Void) => Int -> m Int
benchmark2h n
  | n <= 0 = get
  | otherwise = do
      a <- get
      tell "put"
      put (a + n)
      benchmark2h (n-1)

benchmark2m :: Int -> MTL.StateT Int (MTL.Writer String) Int
benchmark2m n
  | n <= 0 = MTL.get
  | otherwise = do
      a <- MTL.get
      MTL.tell "put"
      MTL.put (a + n)
      benchmark2m (n - 1)

benchHandler2 :: Int -> ((Int, Int), String)
benchHandler2 n = run . runWriter $ runState (benchmark2h n) 0

benchTrans2 :: Int -> ((Int, Int), String)
benchTrans2 n = MTL.runWriter $ MTL.runStateT (benchmark2m n) 0

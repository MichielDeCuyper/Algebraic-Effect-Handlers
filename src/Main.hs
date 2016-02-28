{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import           Benchmarks
import           Criterion.Main

sizeSmall :: Int
sizeSmall = 10^7

sizeMedium :: Int
sizeMedium = 10^8

sizeLarge :: Int
sizeLarge = 10^9

main :: IO ()
main = defaultMain [
  bgroup "count1" [ bench "Handler 1 Small"  $ whnf benchHandler1 sizeSmall
                  , bench "Transf 1 Small"   $ whnf benchTrans1   sizeSmall
                  , bench "Handler 1 Medium" $ whnf benchHandler1 sizeMedium
                  , bench "Transf 1 Medium"  $ whnf benchTrans1   sizeMedium
                  , bench "Handler 1 Large"  $ whnf benchHandler1 sizeLarge
                  , bench "Transf 1 Large"   $ whnf benchTrans1   sizeLarge

                  , bench "Handler 2 Small" $ whnf benchHandler2 sizeSmall
                  , bench "Transf 2 Small"  $ whnf benchTrans2   sizeSmall
                  , bench "Handler 2 Medium" $ whnf benchHandler2 sizeMedium
                  , bench "Transf 2 Medium"  $ whnf benchTrans2   sizeMedium
                  , bench "Handler 2 Large" $ whnf benchHandler2 sizeLarge
                  , bench "Transf 2 Large"  $ whnf benchTrans2   sizeLarge
                  ]]

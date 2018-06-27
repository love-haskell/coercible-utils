{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -O2 #-}

module Main (main) where

import CoercibleUtils
import Data.Monoid (Sum(..))
import Gauge.Main
import Prelude hiding (sum)
import qualified Data.List as List
import qualified Data.Foldable as Foldable

main :: IO ()
main = do
  let nums10, nums100, nums1000, nums10000, nums100000 :: [Int]
      nums10 = [1..10]
      nums100 = [1..100]
      nums1000 = [1..1000]
      nums10000 = [1..10000]
      nums100000 = [1..100000]
  
  defaultMain
    [ bgroup  "lazy sums"
      [ bench "CoercibleUtils.sum:     10"  $ whnf      sumLazy nums10
      , bench "Data.List.sum:          10"  $ whnf List.sum     nums10
      , bench "CoercibleUtils.sum:    100"  $ whnf      sumLazy nums100
      , bench "Data.List.sum:         100"  $ whnf List.sum     nums100
      , bench "CoercibleUtils.sum:   1000"  $ whnf      sumLazy nums1000
      , bench "Data.List.sum:        1000"  $ whnf List.sum     nums1000
      , bench "CoercibleUtils.sum:  10000"  $ whnf      sumLazy nums10000
      , bench "Data.List.sum:       10000"  $ whnf List.sum     nums10000
      , bench "CoercibleUtils.sum: 100000"  $ whnf      sumLazy nums100000
      , bench "Data.List.sum:      100000"  $ whnf List.sum     nums100000
      ]
    
    , bgroup  "strict sums"
      [ bench "CoercibleUtils.sum:     10"  $ whnf      sum nums10
      , bench "Data.List.sum:          10"  $ whnf List.sum nums10
      , bench "CoercibleUtils.sum:    100"  $ whnf      sum nums100
      , bench "Data.List.sum:         100"  $ whnf List.sum nums100
      , bench "CoercibleUtils.sum:   1000"  $ whnf      sum nums1000
      , bench "Data.List.sum:        1000"  $ whnf List.sum nums1000
      , bench "CoercibleUtils.sum:  10000"  $ whnf      sum nums10000
      , bench "Data.List.sum:       10000"  $ whnf List.sum nums10000
      , bench "CoercibleUtils.sum: 100000"  $ whnf      sum nums100000
      , bench "Data.List.sum:      100000"  $ whnf List.sum nums100000
      ]
    ]

sumLazy :: Num a => [a] -> a
{-# INLINE sumLazy #-}
sumLazy xs = ala Sum foldMap xs

sum :: Num a => [a] -> a
{-# INLINE sum #-}
sum xs = ala Sum foldMap' xs

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
{-# INLINE foldMap' #-}
foldMap' f = Foldable.foldl' (\acc x -> acc `mappend` f x) mempty



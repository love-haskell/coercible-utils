{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -O2 #-}

import CoercibleUtils
import Data.Monoid (Sum(..))
import Gauge.Main
import Prelude hiding (sum)
import qualified Data.Foldable as Foldable

main :: IO ()
main = do
  let nums10     = [1..10]
      nums100    = [1..100]
      nums1000   = [1..1000]
      nums10000  = [1..10000]
      nums100000 = [1..100000] 
  defaultMain
    [ bgroup "coercible-utils"
      [ bench "CoercibleUtils.sum: 10"      $ whnf          sum nums10
      , bench "Data.Foldable.sum:  10"      $ whnf Foldable.sum nums10
      , bench "CoercibleUtils.sum: 100"     $ whnf          sum nums100
      , bench "Data.Foldable.sum:  100"     $ whnf Foldable.sum nums100
      , bench "CoercibleUtils.sum: 1000"    $ whnf          sum nums1000
      , bench "Data.Foldable.sum:  1000"    $ whnf Foldable.sum nums1000
      , bench "CoercibleUtils.sum: 10000"   $ whnf          sum nums10000
      , bench "Data.Foldable.sum:  10000"   $ whnf Foldable.sum nums10000
      , bench "CoercibleUtils.sum: 100000"  $ whnf          sum nums100000
      , bench "Data.Foldable.sum:  100000"  $ whnf Foldable.sum nums100000

      
      ]
    ]

sum :: [Int] -> Int
sum xs = ala Sum foldMap xs
{-# INLINE sum #-}

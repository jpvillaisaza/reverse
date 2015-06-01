-- |

module Main (main) where

import Reverse (obverse,reverse)

import Criterion.Main (bench,bgroup,defaultMain,whnf)
import Prelude hiding (reverse)

main :: IO ()
main =
  defaultMain
    [bgroup
       "obverse"
       [bench "6" $ whnf obverse [1..1000000]
       ,bench "7" $ whnf obverse [1..10000000]]
    ,bgroup
       "reverse"
       [bench "6" $ whnf reverse [1..1000000]
       ,bench "7" $ whnf reverse [1..10000000]]]


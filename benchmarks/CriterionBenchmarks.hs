-- | Obverse versus Reverse: criterion benchmarks.

module Main (main) where

import Reverse (obverse,reverse)

import Criterion (bench,bgroup,env,nf)
import Criterion.Main (defaultConfig,defaultMainWith)
import Criterion.Types (Config(reportFile))
import Prelude hiding (reverse)
import Test.QuickCheck (Arbitrary(arbitrary),generate,vectorOf)

-- |
--
-- Generate a list of a given length of arbitrary integers.

arbitraryIntVectorOf :: Int -> IO [Int]
arbitraryIntVectorOf n = generate (vectorOf n arbitrary)

-- |
--
-- Generate two lists of a given length of arbitrary integers.

arbitraryIntVectorsOf :: Int -> IO ([Int],[Int])
arbitraryIntVectorsOf n = do
  xs <- arbitraryIntVectorOf n
  ys <- arbitraryIntVectorOf n
  return (xs,ys)

main :: IO ()
main =
  defaultMainWith
    (defaultConfig {reportFile = Just "obverse-versus-reverse.html"})
    [env (arbitraryIntVectorsOf 100)
         (\ ~(xs,ys) ->
            bgroup
              "obverse versus reverse/100"
              [bench "obverse" $ nf obverse xs
              ,bench "reverse" $ nf reverse ys])
    ,env (arbitraryIntVectorsOf 1000)
         (\ ~(xs,ys) ->
            bgroup
              "obverse versus reverse/1000"
              [bench "obverse" $ nf obverse xs
              ,bench "reverse" $ nf reverse ys])
    ,env (arbitraryIntVectorsOf 10000)
         (\ ~(xs,ys) ->
            bgroup
              "obverse versus reverse/10000"
              [bench "obverse" $ nf obverse xs
              ,bench "reverse" $ nf reverse ys])]

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Obverse versus Reverse: clock benchmarks.

module Main (main) where

import Reverse (obverse,reverse)

import Control.Exception (evaluate)
import Control.Monad (void)
import Data.Time (diffUTCTime,getCurrentTime)
import Formatting ((%),fprint)
import Formatting.Clock (timeSpecs)
import Prelude hiding (reverse)
import System.Clock (Clock(Monotonic),getTime)
import Test.QuickCheck (Arbitrary(arbitrary),generate,vectorOf)

-- |
--
-- Clock something and print the result.
--
-- Example:
--
-- >>> clockSomething (reverse [1..1000])
--
-- See <http://chrisdone.com/posts/measuring-duration-in-haskell>.

clockSomething :: a -> IO ()
clockSomething something = do
  start <- getTime Monotonic
  void (evaluate something)
  end <- getTime Monotonic
  fprint (timeSpecs % "\n") start end

-- |
--
-- Time something and print the result.
--
-- Example:
--
-- >>> timeSomething (reverse [1..1000])

timeSomething :: a -> IO ()
timeSomething something = do
  start <- getCurrentTime
  let !_ = something
  end <- getCurrentTime
  print (diffUTCTime end start)

-- |
--
-- Generate a list of a given length of arbitrary integers.

arbitraryIntVectorOf :: Int -> IO [Int]
arbitraryIntVectorOf n = generate (vectorOf n arbitrary)

main :: IO ()
main = do
  putStrLn "obverse versus reverse/10000000..."
  putStr   "  obverse/1... "
  arbitraryIntVectorOf 10000000 >>= clockSomething . obverse
  putStr   "  reverse/1... "
  arbitraryIntVectorOf 10000000 >>= clockSomething . reverse
  putStr   "  obverse/2... "
  arbitraryIntVectorOf 10000000 >>= clockSomething . obverse
  putStr   "  reverse/2... "
  arbitraryIntVectorOf 10000000 >>= clockSomething . reverse
  putStr   "  obverse/3... "
  arbitraryIntVectorOf 10000000 >>= clockSomething . obverse
  putStr   "  reverse/3... "
  arbitraryIntVectorOf 10000000 >>= clockSomething . reverse
  putStr   "  obverse/4... "
  arbitraryIntVectorOf 10000000 >>= clockSomething . obverse
  putStr   "  reverse/4... "
  arbitraryIntVectorOf 10000000 >>= clockSomething . reverse
  putStr   "  obverse/5... "
  arbitraryIntVectorOf 10000000 >>= clockSomething . obverse
  putStr   "  reverse/5... "
  arbitraryIntVectorOf 10000000 >>= clockSomething . reverse

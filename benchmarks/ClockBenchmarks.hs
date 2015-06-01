{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Main (main) where

import Reverse (obverse,reverse)

import Control.Exception (evaluate)
import Control.Monad (void)
import Data.Time (diffUTCTime,getCurrentTime)
import Formatting (fprint,(%))
import Formatting.Clock (timeSpecs)
import Prelude hiding (reverse)
import System.Clock (Clock(Monotonic),getTime)

-- |
--
-- Example:
--
-- >>> clockSomething (obverse [1..1000])
--
-- See <http://chrisdone.com/posts/measuring-duration-in-haskell>.

clockSomething :: a -> IO ()
clockSomething something = do
  start <- getTime Monotonic
  void (evaluate something)
  end <- getTime Monotonic
  fprint (timeSpecs % "\n") start end

timeSomething :: a -> IO ()
timeSomething something = do
  start <- getCurrentTime
  let !_ = something
  end <- getCurrentTime
  print (diffUTCTime end start)

main :: IO ()
main = do
  clockSomething (obverse [1..10000000])
  clockSomething (reverse [1..10000000])

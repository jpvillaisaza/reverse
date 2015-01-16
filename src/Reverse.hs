{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Reverse.Data.List (obverse,reverse)

import Control.Exception (evaluate)
import Formatting (fprint,(%))
import Formatting.Clock (timeSpecs)
import Prelude hiding (reverse)
import System.Clock (Clock(Monotonic),getTime)

-- |
--
-- Example:
--
-- >>> clock (obverse [1..1000])
--
-- See <http://chrisdone.com/posts/measuring-duration-in-haskell>.

clock :: a -> IO ()
clock a = do
  start <- getTime Monotonic
  evaluate a
  end <- getTime Monotonic
  fprint (timeSpecs % "\n") start end

main :: IO ()
main = putStrLn "Obverse and Reverse"

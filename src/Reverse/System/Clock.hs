{-# LANGUAGE OverloadedStrings #-}

module Reverse.System.Clock (clockSomething) where

import Control.Exception (evaluate)
import Control.Monad (void)
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

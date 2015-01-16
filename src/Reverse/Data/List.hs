------------------------------------------------------------------------------
-- |
-- Module      : Reverse.Data.List
--
--
--
------------------------------------------------------------------------------

module Reverse.Data.List
  (-- $append
   obverse
  ,reverse)
  where

import Prelude hiding (reverse)

-- $append
--
--   [Identity]
--
--       prop> [] ++ xs == xs
--       prop> xs ++ [] == xs
--
--   [Associativity]
--
--       prop> xs ++ ys ++ zs == (xs ++ ys) ++ zs

-- |
--
-- Examples:
--
-- >>> obverse [9,8..0]
-- [0,1,2,3,4,5,6,7,8,9]
--
-- Properties:
--
-- prop> xs == obverse (obverse xs)

obverse :: [a] -> [a]
obverse []     = []
obverse (x:xs) = obverse xs ++ [x]

-- |
--
-- Examples:
--
-- >>> reverse [0..9]
-- [9,8,7,6,5,4,3,2,1,0]
--
-- Properties:
--
-- prop> xs == reverse (reverse xs)

reverse :: [a] -> [a]
reverse xs = reverse' xs []
  where
    reverse' []     ys = ys
    reverse' (x:xs) ys = reverse' xs (x:ys)

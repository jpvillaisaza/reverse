module Reverse.Data.List where

import Prelude ((++))

-- reverse [1..10000] 3.12 3.07

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

-- 0.22 0.22

reverse' :: [a] -> [a]
reverse' xs = reverse'' xs []
  where
    reverse'' []     ys = ys
    reverse'' (x:xs) ys = reverse'' xs (x:ys)

-- 0.23 0.23

reverse'' :: [a] -> [a]
reverse'' xs = reverse''' xs []

reverse''' :: [a] -> [a] -> [a]
reverse''' []     ys = ys
reverse''' (x:xs) ys = reverse''' xs (x:ys)

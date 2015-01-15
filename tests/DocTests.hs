module Main (main) where

import Test.DocTest (doctest)

main :: IO ()
main = doctest ["-isrc", "src/Reverse/Data/List.hs"]

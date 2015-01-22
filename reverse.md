Obverse and Reverse
===================



```haskell
obverse :: [a] -> [a]
obverse []     = []
obverse (x:xs) = obverse xs ++ [x]
```


```haskell
reverse :: [a] -> [a]
reverse xs = reverse' xs []
  where
    reverse' []      ys = ys
    reverse' (x:xs') ys = reverse' xs' (x:ys)
```

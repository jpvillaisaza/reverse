Obverse and Reverse
===================



```haskell
obverse :: [a] -> [a]
obverse []     = []
obverse (x:xs) = obverse xs ++ [x]
```

```haskell
reverse' :: [a] -> [a] -> [a]
reverse' xs ys = obverse xs ++ ys
```

```
obverse xs == reverse' xs []
```

```
reverse' [] ys
  = ()
obverse [] ++ ys
  = ()
[] ++ ys
  = ()
ys
```

```
reverse' (x:xs) ys
  = ()
obverse (x:xs) ++ ys
  = ()
(obverse xs ++ [x]) ++ ys
  = ()
obverse xs ++ [x] ++ ys
  = ()
obverse xs ++ (x:ys)
  = ()
reverse' xs (x:ys)
```

```haskell
reverse'' :: [a] -> [a] -> [a]
reverse'' []     ys = ys
reverse'' (x:xs) ys = reverse'' xs (x:ys)
```

```
[] ++ xs == xs
xs ++ [] == xs
xs ++ ys ++ zs == (xs ++ ys) ++ zs
```

```haskell
reverse :: [a] -> [a]
reverse xs = reverse' xs []
  where
    reverse' []      ys = ys
    reverse' (x:xs') ys = reverse' xs' (x:ys)
```

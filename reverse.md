Obverse and Reverse
===================



```haskell
obverse :: [a] -> [a]
obverse []     = []
obverse (x:xs) = obverse xs ++ [x]
```

```
> clockSomething (obverse [1..1000000] :: [Integer])
1.20 s
> clockSomething (obverse [1..10000000] :: [Integer])
11.66 s
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

```
> clockSomething (reverse [1..1000000] :: [Integer])
493.63 ms
> clockSomething (reverse [1..10000000] :: [Integer])
5.08 s
```

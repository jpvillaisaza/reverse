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
  = (by definition of reverse')
obverse [] ++ ys
  = (by definition of obverse)
[] ++ ys
  = (by left identity)
ys
```

```
reverse' (x:xs) ys
  = (by definition of reverse')
obverse (x:xs) ++ ys
  = (by definition of obverse)
(obverse xs ++ [x]) ++ ys
  = (by associativity)
obverse xs ++ [x] ++ ys
  = (by)
obverse xs ++ (x:ys)
  = (by definition of reverse')
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

Obverse and Reverse
===================

> Still other examples will show not only the harmony between obverse
> and reverse, but how coins were dedicated to more than one divinity.

&mdash;Isaac Bassett Choate

(See <https://github.com/jpvillaisaza/reverse>.)

In section 7.5 of Thinking Functionally with Haskell, Richard Bird
uses the `reverse` function as an example of improving the running
time of a function by adding an accumulating parameter to it. He
begins with a simple and inefficient definition of `reverse`, which
we'll call `obverse`:

```haskell
obverse :: [a] -> [a]
obverse []     = []
obverse (x:xs) = obverse xs ++ [x]
```

In order to get a better idea of the inefficiency of this function,
let's clock it:

```
> clockSomething (obverse [1..1000000] :: [Integer])
1.20 s
> clockSomething (obverse [1..10000000] :: [Integer])
11.66 s
```

In search of a more efficient definition of `obverse`, Bird defines an
auxiliary function, `reverse'`, as follows:

```haskell
reverse' :: [a] -> [a] -> [a]
reverse' xs ys = obverse xs ++ ys
```

Clearly,

```
obverse xs == reverse' xs []
```

An efficient definition of `reverse'` can be calculated by induction.
In the case of an empty list,

`reverse' [] ys`  
&nbsp;&nbsp;`==` (by definition of `reverse'`)  
`obverse [] ++ ys`  
&nbsp;&nbsp;`==` (by definition of `obverse`)  
`[] ++ ys`  
&nbsp;&nbsp;`==` (by left identity)  
`ys`

Otherwise,

`reverse' (x:xs) ys`  
&nbsp;&nbsp;`==` (by definition of `reverse'`)  
`obverse (x:xs) ++ ys`  
&nbsp;&nbsp;`==` (by definition of `obverse`)  
`(obverse xs ++ [x]) ++ ys`  
&nbsp;&nbsp;`==` (by associativity)  
`obverse xs ++ [x] ++ ys`  
&nbsp;&nbsp;`==` (by definition of `(++)` and left identity)  
`obverse xs ++ (x:ys)`  
&nbsp;&nbsp;`==` (by definition of `reverse'`)  
`reverse' xs (x:ys)`

The efficient definition of `reverse'` can be used to define an
efficient `reverse` function, as follows:

```haskell
reverse :: [a] -> [a]
reverse xs = reverse' xs []
  where
    reverse' []      ys = ys
    reverse' (x:xs') ys = reverse' xs' (x:ys)
```

In order to compare the efficiency of the `obverse` and `reverse`
functions, let's clock the latter too:

```
> clockSomething (reverse [1..1000000] :: [Integer])
493.63 ms
> clockSomething (reverse [1..10000000] :: [Integer])
5.08 s
```

This is not just an example of how to improve a program's performance,
but an example of how to use mathematics to find a better algorithm,
which is “the best way to improve a programs's performance.” We're
merely using definitions and the fact that lists, the empty list, and
the append function form a monoid, but we could do so much more. In
short, it's just an example of how to use mathematics as one way to
better understand and improve our programs.

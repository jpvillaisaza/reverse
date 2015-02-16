Obverse and Reverse
===================

> Still other examples will show not only the harmony between obverse
> and reverse, but how coins were dedicated to more than one divinity.

(See <https://github.com/jpvillaisaza/reverse>.)

In Thinking Functionally with Haskell, Richard Bird uses the `reverse`
function as an example of improving the running time of a computation
by adding an accumulating parameter to a function. In doing so, he
actually shows an example of how to use mathematics to calculate a
better algorithm for reverse, which is the best way to improve a
program's performance.

Bird begins with a simple and inefficient definition of `reverse`,
which we'll call `obverse`. Basically, if we want to... obverse a
list, we simply obverse its tail and then append its head:

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
&nbsp;&nbsp;`==` (by)  
`obverse xs ++ (x:ys)`  
&nbsp;&nbsp;`==` (by definition of `reverse'`)  
`reverse' xs (x:ys)`

Now, the efficient definition of `reverse'` can be used to define an
efficient `reverse` function, as follows:

```haskell
reverse :: [a] -> [a]
reverse xs = reverse' xs []
  where
    reverse' []      ys = ys
    reverse' (x:xs') ys = reverse' xs' (x:ys)
```

And this function is more efficient than the `obverse` function:

```
> clockSomething (reverse [1..1000000] :: [Integer])
493.63 ms
> clockSomething (reverse [1..10000000] :: [Integer])
5.08 s
```

Besides definitions, we're using the fact that lists, the empty list
and append form a monoid to calculate a better algorithm and improve
the performance of reverse. And it's just one of Bird's examples of
how to use mathematics to better understand and improve our programs.
We can move forward and use things like the functor laws and
parametricity to improve our programs. We won't be using reverse, but
it's a good example.

he actually shows an example of calculating a function with
mathematics and says that “the best way to improve a program's
performance is to use a better algorithm.”

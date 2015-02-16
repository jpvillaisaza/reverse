Obverse and Reverse
===================

> Still other examples will show not only the harmony between obverse
> and reverse, but how coins were dedicated to more than one divinity.

<https://github.com/jpvillaisaza/reverse>

In Thinking Functionally with Haskell, Richard Bird uses the `reverse`
function as an example of improving the running time of a computation
by adding an accumulating parameter to a function. In doing so, he
actually shows an example of how to use mathematics to calculate a
better algorithm for reverse, which is the best way to improve a
program's performance.

We start with the most basic definition of reverse, except we call it
obverse... This way of defining reverse is simple and very clear in
terms of functional programming. If we want to reverse a list, we
simply reverse its tail and append to that the head.

```haskell
obverse :: [a] -> [a]
obverse []     = []
obverse (x:xs) = obverse xs ++ [x]
```

Besides simple, this definition is very inefficient, quadratic. Just
as an example of how to measure time in Haskell, we clock the use of
obverse:

```
> clockSomething (obverse [1..1000000] :: [Integer])
1.20 s
> clockSomething (obverse [1..10000000] :: [Integer])
11.66 s
```

Now, suppose an auxiliary function with an extra parameter which will
take us to a better reverse.

```haskell
reverse' :: [a] -> [a] -> [a]
reverse' xs ys = obverse xs ++ ys
```

Clearly,

```
obverse xs == reverse' xs []
```

We can calculate an efficient version of `reverse'` as follows. In the
case of an empty list,


`reverse' [] ys`  
&nbsp;&nbsp;`==` (by definition of `reverse'`)  
`obverse [] ++ ys`  
&nbsp;&nbsp;`==` (by definition of `obverse`)  
`[] ++ ys`  
&nbsp;&nbsp;`==` (by left identity)  
`ys`

Otherwise:

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

To sum up, we have a new auxiliary reverse function:

```haskell
reverse'' :: [a] -> [a] -> [a]
reverse'' []     ys = ys
reverse'' (x:xs) ys = reverse'' xs (x:ys)
```

And we can use it to define reverse in another way, a better way.

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

And this function is more efficient, it's linear. Let's compare the
running times of obverse and reverse.

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

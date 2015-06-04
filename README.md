# Obverse and Reverse

[![Build Status][build-status-image]][build-status]

> Obverse, or reverse, I can make nothing out of it.

&mdash;Mohi

```
git clone https://github.com/stackbuilders/reverse.git
cd reverse/
```

```
cabal sandbox init
```

```
cabal install --only-dependencies --enable-tests --enable-benchmarks
cabal configure --enable-tests --enable-benchmarks
cabal build
```

## Tests

Run all tests in the test suite:

```
cabal test doctests
```

## Benchmarks

Finally, run all (`cabal bench`) or specific benchmarks.

First, run the clock benchmarks:

```
$ cabal bench clock-benchmarks
...
Benchmark clock-benchmarks: RUNNING...
obverse versus reverse/10000000...
  obverse/1... 3.63 s
  reverse/1... 1.85 s
  obverse/2... 4.44 s
  reverse/2... 1.33 s
  obverse/3... 3.30 s
  reverse/3... 2.27 s
  obverse/4... 3.22 s
  reverse/4... 2.14 s
  obverse/5... 3.38 s
  reverse/5... 1.32 s
Benchmark clock-benchmarks: FINISH
```

Second, run the criterion benchmarks:

```
cabal bench criterion-benchmarks
...
Benchmark criterion-benchmarks: RUNNING...
benchmarking obverse versus reverse/100/obverse
time                 35.82 μs   (35.34 μs .. 36.44 μs)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 36.06 μs   (35.66 μs .. 36.50 μs)
std dev              1.406 μs   (1.218 μs .. 1.591 μs)
variance introduced by outliers: 43% (moderately inflated)

benchmarking obverse versus reverse/100/reverse
time                 919.8 ns   (912.1 ns .. 930.7 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 936.5 ns   (925.9 ns .. 949.1 ns)
std dev              40.75 ns   (35.83 ns .. 44.23 ns)
variance introduced by outliers: 60% (severely inflated)

benchmarking obverse versus reverse/1000/obverse
time                 5.994 ms   (5.934 ms .. 6.050 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 5.987 ms   (5.964 ms .. 6.021 ms)
std dev              81.83 μs   (58.04 μs .. 130.1 μs)

benchmarking obverse versus reverse/1000/reverse
time                 9.551 μs   (9.495 μs .. 9.633 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 9.510 μs   (9.490 μs .. 9.558 μs)
std dev              92.05 ns   (52.81 ns .. 165.5 ns)

benchmarking obverse versus reverse/10000/obverse
time                 1.520 s    (1.499 s .. 1.537 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.518 s    (1.514 s .. 1.520 s)
std dev              3.648 ms   (0.0 s .. 4.206 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking obverse versus reverse/10000/reverse
time                 133.6 μs   (133.1 μs .. 134.4 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 134.7 μs   (133.9 μs .. 136.1 μs)
std dev              3.611 μs   (2.359 μs .. 5.243 μs)
variance introduced by outliers: 22% (moderately inflated)

Benchmark criterion-benchmarks: FINISH
```

[build-status]: https://travis-ci.org/stackbuilders/reverse
[build-status-image]: https://travis-ci.org/stackbuilders/reverse.svg?branch=master

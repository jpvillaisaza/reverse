Obverse and Reverse [![Build Status][build-status-image]][build-status]
===================

> Obverse, or reverse, I can make nothing out of it.

```
git clone https://github.com/jpvillaisaza/reverse.git
cd reverse/
```

```
cabal sandbox init
cabal install --only-dependencies --enable-tests
cabal configure --enable-tests
cabal build
cabal test
```

[build-status]: https://travis-ci.org/jpvillaisaza/reverse
[build-status-image]: https://travis-ci.org/jpvillaisaza/reverse.svg?branch=master

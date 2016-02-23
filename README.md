# problem535 - mcandre's attempt at Project Euler problem 535

https://projecteuler.net/problem=535

Or see index.html if resource is down.

# REQUIREMENTS

* [Haskell](https://www.haskell.org/) (64+ bits of integer precision)

E.g. `brew install ghc`

## Optional

* [Make](https://www.gnu.org/software/make/)
* [Ruby](http://www.ruby-lang.org/en/) (for guard)

E.g., `bundle`

# EXAMPLE

```
$ make
time bin/Problem535
S(1:20)_given:
[1,1,2,1,3,2,4,1,5,3,6,2,7,8,4,9,1,10,11,5]
S(1:20):
[1,1,2,1,3,2,4,1,5,3,6,2,7,8,4,9,1,10,11,5]
T(1)_given:
1
T(1):
1
T(20)_given:
86
T(20):
86
T(1000)_given:
364089
T(1000):
364089
T(10^9)_given:
498676527978348241
T(10^9):
498676527978348241
      549.30 real       199.72 user       162.96 sys
```

On a 16 GB RAM 2.3 GHz Intel Core i7 15" Mid-2012 MacBook Pro

# GUARD

```
$ vi Problem535.hs
...
$ guard
...
```

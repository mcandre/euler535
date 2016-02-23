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
mkdir -p bin
ghc -O2 -Wall -fwarn-tabs --make -fforce-recomp -o bin/problem535 problem535.hs
[1 of 1] Compiling Main             ( problem535.hs, problem535.o )

problem535.hs:43:1: Warning: Defined but not used: ‘t'’
Linking bin/problem535 ...
time bin/problem535
S(1:20)_given:
[1,1,2,1,3,2,4,1,5,3,6,2,7,8,4,9,1,10,11,5]
S(1:20):
[1,1,2,1,3,2,4,1,5,3,6,2,7,8,4,9,1,10,11,5]
...
```

# GUARD

```
$ vi problem535.hs
...
$ guard
...
```

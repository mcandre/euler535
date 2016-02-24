# problem535 - mcandre's attempt at Project Euler problem 535

https://projecteuler.net/problem=535

Or see index.html if resource is down.

# EXAMPLE

```
$ gradle shadowJar
$ time java -Xmx16G -jar build/libs/problem535-all.jar
S(1:20)_given:	[1, 1, 2, 1, 3, 2, 4, 1, 5, 3, 6, 2, 7, 8, 4, 9, 1, 10, 11, 5]
S(1:20):	[1, 1, 2, 1, 3, 2, 4, 1, 5, 3, 6, 2, 7, 8, 4, 9, 1, 10, 11, 5]
T(1)_given:	1
T(1):		1
T(20)_given:	86
T(20):		86
T(1000)_given:	364089
T(1000):	364089
T(10^9)_given	498676527978348241
T(10^9):	498676527978348241

real	1m37.980s
user	1m9.300s
sys	0m46.361s
```

On an 8 GB RAM 1.7 GHz Intel Core i7 13" Mid-2013 MacBook Air

# REQUIREMENTS

* [Java](http://www.oracle.com/technetwork/java/javase/downloads/index.html) 1.8+

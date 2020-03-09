LeanCheck Memory Usage
======================

Due to the way it is implemented (by using simple lists of lists for
enumeration), LeanCheck can be quite memory intensive.

When running LeanCheck for 500, 1 000 or 10 000 values on each property one
should be fine with regard to memory usage.  However, if as we start going
towards 1 000 000 test values per property, memory consumption can be quite
high depending on the types being tested.

Here are tables with LeanCheck's memory consumption for a few standard Haskell
types.


TODO: finish formatting me


Bool
----

```
n   time  memory
1     0s     17M
```

Int
---

```
       n  time  memory
       1  0.0s  17M
      10  0.0s  17M
     100  0.0s  17M
    1000  0.0s  17M
   10000  0.0s  18M
  100000  0.0s  19M
 1000000  0.0s  18M
10000000  0.2s  19M
```


(Int,Int)
---------

```
       1  0.0s  17M
      10  0.0s  18M
     100  0.0s  18M
    1000  0.0s  18M
   10000  0.0s  19M
  100000  0.0s  19M
 1000000  0.1s  19M
10000000  2.1s  21M
```

(Int,Int,Int)
-------------

```
       1  0.0s  17M
      10  0.0s  17M
     100  0.0s  17M
    1000  0.0s  17M
   10000  0.0s  18M
  100000  0.0s  19M
 1000000  0.1s  22M
10000000  1.5s  34M
```


[()]
----

```
    1   0.0s  17M
   10   0.0s  17M
  100   0.0s  18M
 1000   0.1s  19M
10000  24.9s  29M
```


[Bool]
------

```
       1  0.0s   17M
      10  0.0s   17M
     100  0.0s   17M
    1000  0.0s   17M
   10000  0.0s   19M
  100000  0.1s   30M
 1000000  0.4s  119M
10000000  4.3s    1G
```


[Int]
-----


```
       1  0.0s   17M
      10  0.0s   17M
     100  0.0s   17M
    1000  0.0s   17M
   10000  0.0s   20M
  100000  0.1s   41M
 1000000  0.5s  205M
10000000  6.6s    2G
```



[[Bool]]
--------

```
       1  0.0s    17M
      10  0.0s    17M
     100  0.0s    17M
    1000  0.0s    18M
   10000  0.0s    18M
  100000  0.1s    34M
 1000000  0.5s   204M
10000000  4.7s     2G
```


[[Int]]
-------

```
       1  0.0s    17M
      10  0.0s    17M
     100  0.0s    17M
    1000  0.0s    18M
   10000  0.0s    20M
  100000  0.1s    35M
 1000000  0.5s   193M
10000000  4.7s     2G
```


[(Int,Int)]
-----------

```
       1  0.0s    17M
      10  0.0s    17M
     100  0.0s    17M
    1000  0.0s    18M
   10000  0.0s    19M
  100000  0.1s    34M
 1000000  0.5s   166M
10000000  4.7s     1G
```


Generating measurements from this report
----------------------------------------

Values in the report above are approximate.  If you need more precise values,
you will need to generate them yourself.

To generate data from this report yourself, just:

```
$ make bench/pick
$ ./bench/memory-usage
...
```

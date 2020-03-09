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
1     0s     17k
```

Int
---

```
       n  time  memory
       1  0.0s  17k
      10  0.0s  17k
     100  0.0s  17k
    1000  0.0s  17k
   10000  0.0s  18k
  100000  0.0s  19k
 1000000  0.0s  18k
10000000  0.2s  19k
```


(Int,Int)
---------

```
       1  0.0s  17k
      10  0.0s  18k
     100  0.0s  18k
    1000  0.0s  18k
   10000  0.0s  19k
  100000  0.0s  19k
 1000000  0.1s  19k
10000000  2.1s  21k
```

(Int,Int,Int)
-------------

```
       1  0.0s  17k
      10  0.0s  17k
     100  0.0s  17k
    1000  0.0s  17k
   10000  0.0s  18k
  100000  0.0s  19k
 1000000  0.1s  22k
10000000  1.5s  34k
```


[()]
----

```
    1   0.0s  17k
   10   0.0s  17k
  100   0.0s  18k
 1000   0.1s  19k
10000  24.9s  29k
```


[Bool]
------

```
       1  0.0s   17k
      10  0.0s   17k
     100  0.0s   17k
    1000  0.0s   17k
   10000  0.0s   19k
  100000  0.1s   30k
 1000000  0.4s  119k
10000000  4.3s  908k
```


[Int]
-----


```
       1  0.0s   17k
      10  0.0s   17k
     100  0.0s   17k
    1000  0.0s   17k
   10000  0.0s   20k
  100000  0.1s   41k
 1000000  0.5s  205k
10000000  6.6s    2M
```



[[Bool]]
--------

```
       1  0.00    17k
      10  0.00    17k
     100  0.00    17k
    1000  0.00    18k
   10000  0.01    18k
  100000  0.04    34k
 1000000  0.50   204k
10000000  4.75  1661k
```


[[Int]]
-------

```
1 0.00 17352
10 0.00 17288
100 0.00 17380
1000 0.00 18284
10000 0.00 20256
100000 0.04 35924
1000000 0.50 193744
10000000 4.73 1682480
```


[(Int,Int)]
-----------

```
1 0.00 17696
10 0.00 17732
100 0.00 17588
1000 0.00 18248
10000 0.00 19568
100000 0.05 34756
1000000 0.48 166624
10000000 4.76 1533092
```


Note
----

To generate data from this report yourself, just:

```
$ make bench/pick
$ ./bench/memory-usage
...
```

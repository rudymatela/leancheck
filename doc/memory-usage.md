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
10000000  0.1s  19k
```


(Int,Int)
---------

```
(Int,Int) 1 0.00 17364
(Int,Int) 10 0.00 17672
(Int,Int) 100 0.00 17476
(Int,Int) 1000 0.00 17988
(Int,Int) 10000 0.00 18792
(Int,Int) 100000 0.01 18696
(Int,Int) 1000000 0.09 19076
(Int,Int) 10000000 2.09 21400
```

(Int,Int,Int)
-------------

```
(Int,Int,Int) 1 0.00 17448
(Int,Int,Int) 10 0.00 17660
(Int,Int,Int) 100 0.00 17700
(Int,Int,Int) 1000 0.00 17820
(Int,Int,Int) 10000 0.00 18432
(Int,Int,Int) 100000 0.00 19276
(Int,Int,Int) 1000000 0.08 22124
(Int,Int,Int) 10000000 1.54 34700
```


[()] 1 0.00 17356
[()] 10 0.00 17304
[()] 100 0.00 18768
[()] 1000 0.07 19836
[()] 10000 24.90 29596
[Bool] 1 0.00 17684
[Bool] 10 0.00 17276
[Bool] 100 0.00 17488
[Bool] 1000 0.00 17588
[Bool] 10000 0.00 19708
[Bool] 100000 0.03 30276
[Bool] 1000000 0.39 119400
[Bool] 10000000 4.35 908188
[Int] 1 0.00 17620
[Int] 10 0.00 17600
[Int] 100 0.00 17456
[Int] 1000 0.00 17952
[Int] 10000 0.01 20856
[Int] 100000 0.05 41692
[Int] 1000000 0.53 205704
[Int] 10000000 6.63 2157664
[[Bool]] 1 0.00 17352
[[Bool]] 10 0.00 17276
[[Bool]] 100 0.00 17224
[[Bool]] 1000 0.00 18164
[[Bool]] 10000 0.01 18880
[[Bool]] 100000 0.04 34268
[[Bool]] 1000000 0.50 204680
[[Bool]] 10000000 4.75 1661892
[[Int]] 1 0.00 17352
[[Int]] 10 0.00 17288
[[Int]] 100 0.00 17380
[[Int]] 1000 0.00 18284
[[Int]] 10000 0.00 20256
[[Int]] 100000 0.04 35924
[[Int]] 1000000 0.50 193744
[[Int]] 10000000 4.73 1682480
[(Int,Int)] 1 0.00 17696
[(Int,Int)] 10 0.00 17732
[(Int,Int)] 100 0.00 17588
[(Int,Int)] 1000 0.00 18248
[(Int,Int)] 10000 0.00 19568
[(Int,Int)] 100000 0.05 34756
[(Int,Int)] 1000000 0.48 166624
[(Int,Int)] 10000000 4.76 1533092



Note
----

To generate data from this report yourself, just:

```
$ make bench/pick
$ ./bench/memory-usage
...
```

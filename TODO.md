TO DO list for LeanCheck
========================

List of things to do for LeanCheck.


misc
----

* improve `mk/haskell.mk`: pass ALLHS and LIBHS instead of HSS
  By making that distinction, haskell.mk will be able to handle Haddock.
  It will also be clearer what each parameter means.
  Note that ALLHS and LIBHS are not (but could be) the final names.

* parameterize number of tests in test programs and add slow-test target


documentation
-------------

* on tutorial.md, write about how to create test programs;

* on data-invariant.md, write missing section;


other improvements
------------------

* add `classify` function to measure distribution of data:
  something like:

    module Test.LeanCheck.Stats

    -- top-level functions
    classStringStats      :: Listable a => Int -> (a -> String) -> IO ()
    classStringStatsT     :: Listable a => Int -> (a -> String) -> IO ()
    classStats  :: (Listable a, Show b) => Int -> (a -> b)      -> IO ()
    classStatsT :: (Listable a, Show b) => Int -> (a -> b)      -> IO ()
    conditionStats        :: Listable a => Int -> [a->Bool]     -> IO ()
    conditionStatsT       :: Listable a => Int -> [a->Bool]     -> IO ()

    -- auxiliary functions
    classifyOn :: Eq b => (a -> b) -> [a] -> [[a]]
    classifyBy :: (a -> a -> Bool) -> [a] -> [[a]]
    classify   :: Eq a =>             [a] -> [[a]]
    countsBy   :: (a -> a -> Bool) -> [a] -> [(a,Int)]
    countsOn   :: Eq b => (a -> b) -> [a] -> [(b,Int)]
    counts     :: Eq a =>             [a] -> [(a,Int)]


later
-----

* implement stub `Test.LeanCheck.Function.*` modules;

* somehow, improve the improve the enumeration of `Char`s:

   list = [ ['a'], ['b','c'], ['d','e','f'], ... ]
      ||| [ [' '], ['\n'] ]
      ||| [ ['0'], ['1'], ['2'], ...]
      ||| ...
     where
     ||| is something that interleaves tiers of different lists...

* further improve showing of functions, indead of showing:

    \p q -> case (p,q) of
            (False,False) -> False
            (False,True)  -> False
            (True,False)  -> True
            (True,True)   -> False

  actually show just:

    \p q -> case (p,q) of
            (True,False)  -> True
            _             -> False

  Some thinking may have to be done for:

    \x y -> case (x,y) of
            (0,0) -> 0
            (0,1) -> 0
            (1,1) -> 1
            (1,0) -> 1
            (0,2) -> 0
            (1,1) -> 1
            (2,0) -> 1
            ...

  Where: `const (const 1) ->/ [(1,const 0)]`.

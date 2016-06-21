Using LeanCheck types with a data invariant
-------------------------------------------

Some datatypes follow a data invariant / precondition, e.g.:
  AVL and Red-Black trees must be balanced;
  a [`Rational`] should be simplified and have a non-zero denominator;
  a set representation by a list should be ordered.

For the following `Set` datatype with insertion and membership test:

    -- A simple set representation by a strictly ordered list
    data Set a = Set [a]
      deriving (Eq, Show)

    -- data invariant for the Set type
    okSet :: Ord a => Set a -> Bool
    okSet (Set xs) = sord xs
      where
      sord (x:y:xs) = x < y && sord (y:xs)
      sord _        = True

    insertS :: Ord a => a -> Set a -> Set a
    insertS x (Set xs) = Set $ insert x xs

    elemS :: Ord a => a -> Set a -> Bool
    elemS x (Set xs) = elem x xs

By defining [`Listable`] naively

    instance (Ord a, Listable a) => Listable (Set a) where
      tiers = cons1 Set

we get invalid sets when we [`list`] sets.  On ghci:

    > take 5 (list :: [Set Int])
    [Set [],Set [0],Set [0,0],Set [1],Set [0,0,0]]
    > map okSet $ take 5 (list :: [Set Int])
    [True,True,False,True,False]

Both `Set [0,0]` and `Set [0,0,0]`, despite being type-correct, are invalid
sets as they do not follow the data invariant `okSet`.  To resolve that, we
have three solutions:

1. **Prefix all properties with a precondition** (uglier and inefficient):

        prop_elemInsertS :: Ord a => a -> Set a -> Bool
        prop_elemInsertS x s = okSet s ==> x `elemS` (x `insertS` s)

2. **Filter invalid values in the Listable instance** (elegant but inefficient):

    We can use the [`suchThat`] function when declaring `tiers`:

        instance (Ord a, Listable a) => Listable (Set a) where
          tiers = cons1 Set `suchThat` okSet

    Now only valid sets are listed:

        > take 5 (list :: [Set Int])
        [Set [],Set [0],Set [1],Set [0,1],Set [-1]]

    And we can simply write our property as:

        prop_elemInsertS x s = x `elemS` (x `insertS` s)


3. **Only generate valid values in the Listable instance** (elegant and efficient):

    TODO: write!

[`Listable`]: https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#t:Listable
[`list`]:     https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:list
[`suchThat`]: https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:suchThat
[`Rational`]: https://hackage.haskell.org/package/base/docs/Data-Ratio.html#t:Ratio

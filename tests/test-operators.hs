-- Copyright (c) 2015-2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import System.Exit (exitFailure)
import Data.List (elemIndices,sort)
import Test.LeanCheck
import Test.LeanCheck.Utils

import Data.List (isPrefixOf)
import Data.Function (on)

main :: IO ()
main =
  case elemIndices False (tests 100) of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n $ (not . not) === id
  , fails n $ abs === (* (-1))             -:> int
  , holds n $ (+) ==== (\x y -> sum [x,y]) -:> int
  , fails n $ (+) ==== (*)                 -:> int

  , holds n $ const True  &&& const True  -:> bool
  , fails n $ const False &&& const True  -:> int
  , holds n $ const False ||| const True  -:> int
  , fails n $ const False ||| const False -:> int

  , holds n $ commutative (+)  -:> int
  , holds n $ commutative (*)  -:> int
  , holds n $ commutative (++) -:> [()]
  , holds n $ commutative (&&)
  , holds n $ commutative (||)
  , fails n $ commutative (-)  -:> int
  , fails n $ commutative (++) -:> [bool]
  , fails n $ commutative (==>)

  , holds n $ associative (+)  -:> int
  , holds n $ associative (*)  -:> int
  , holds n $ associative (++) -:> [int]
  , holds n $ associative (&&)
  , holds n $ associative (||)
  , fails n $ associative (-)  -:> int
  , fails n $ associative (==>)

  , holds n $ distributive (*) (+) -:> int
  , fails n $ distributive (+) (*) -:> int

  , holds n $ symmetric (==) -:> int
  , holds n $ symmetric (/=) -:> int
  , fails n $ symmetric (<=) -:> int

  , holds n $   reflexive (==) -:> int
  , holds n $ irreflexive (/=) -:> int

  , holds n $ (<)  `symmetric2` (>)  -:> int
  , holds n $ (<=) `symmetric2` (>=) -:> int
  , fails n $ (<)  `symmetric2` (>=) -:> int
  , fails n $ (<=) `symmetric2` (>)  -:> int

  , holds n $ transitive (==) -:> bool
  , holds n $ transitive (<)  -:> bool
  , holds n $ transitive (<=) -:> bool
  , fails n $ transitive (/=) -:> bool
  , holds n $ transitive (==) -:> int
  , holds n $ transitive (<)  -:> int
  , holds n $ transitive (<=) -:> int
  , fails n $ transitive (/=) -:> int

  , holds n $ asymmetric    (<)  -:> int
  , holds n $ antisymmetric (<=) -:> int
  , fails n $ asymmetric    (<=) -:> int
  , holds n $ asymmetric    (>)  -:> int
  , holds n $ antisymmetric (>=) -:> int
  , fails n $ asymmetric    (>=) -:> int

  , holds n $ equivalence (==) -:> int
  , holds n $ equivalence ((==) `on` fst) -:> (int,int)
  , holds n $ equivalence ((==) `on` length) -:> [int]

  , holds n $       totalOrder (<=) -:> int
  , holds n $ strictTotalOrder (<)  -:> int
  , fails n $       totalOrder (<)  -:> int
  , fails n $ strictTotalOrder (<=) -:> int
  , holds n $       totalOrder (>=) -:> int
  , holds n $ strictTotalOrder (>)  -:> int
  , fails n $       totalOrder (>)  -:> int
  , fails n $ strictTotalOrder (>=) -:> int

  , holds n $ partialOrder isPrefixOf -:> [int]
  , fails n $   totalOrder isPrefixOf -:> [int]

  , holds n $ comparison compare -:> int
  , holds n $ comparison compare -:> bool
  , holds n $ comparison compare -:> ()

  , holds n $ okEqOrd -:> ()
  , holds n $ okEqOrd -:> int
  , holds n $ okEqOrd -:> char
  , holds n $ okEqOrd -:> bool
  , holds n $ okEqOrd -:> [()]
  , holds n $ okEqOrd -:> [int]
  , holds n $ okEqOrd -:> [bool]
  , holds n $ okEqOrd -:> float  -- fails if NaN is included in enumeration
  , holds n $ okEqOrd -:> double -- fails if NaN is included in enumeration
  , holds n $ okEqOrd -:> rational

  -- TODO: fix the following tests:
  , holds n $ okNum -:> int
  , holds n $ okNum -:> integer
--, holds n $ okNum -:> float   -- Infinity makes this fail
--, holds n $ okNum -:> double  -- Infinity makes this fail
  , holds n $ okNum -:> rational

  , holds n $ idempotent id   -:> int
  , holds n $ idempotent abs  -:> int
  , holds n $ idempotent sort -:> [bool]
  , fails n $ idempotent not

  , holds n $ identity id   -:> int
  , holds n $ identity (+0) -:> int
  , holds n $ identity sort -:> [()]
  , holds n $ identity (not . not)
  , fails n $ identity not

  , holds n $ neverIdentity not
  , fails n $ neverIdentity abs    -:> int
  , fails n $ neverIdentity negate -:> int
  ]

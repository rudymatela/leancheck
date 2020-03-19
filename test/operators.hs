-- Copyright (c) 2015-2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import System.Exit (exitFailure)
import Data.List (elemIndices,sort)
-- import Test.LeanCheck -- already exported by Test
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

  , holds n $ isCommutative (+)  -:> int
  , holds n $ isCommutative (*)  -:> int
  , holds n $ isCommutative (++) -:> [()]
  , holds n $ isCommutative (&&)
  , holds n $ isCommutative (||)
  , fails n $ isCommutative (-)  -:> int
  , fails n $ isCommutative (++) -:> [bool]
  , fails n $ isCommutative (==>)

  , holds n $ isAssociative (+)  -:> int
  , holds n $ isAssociative (*)  -:> int
  , holds n $ isAssociative (++) -:> [int]
  , holds n $ isAssociative (&&)
  , holds n $ isAssociative (||)
  , fails n $ isAssociative (-)  -:> int
  , fails n $ isAssociative (==>)

  , holds n $ isDistributive (*) (+) -:> int
  , fails n $ isDistributive (+) (*) -:> int

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
  , holds n $ okEqOrd -:> nat
  , holds n $ okEqOrd -:> natural

  , holds n $ okNum -:> int
  , holds n $ okNum -:> integer
-- NOTE: the following two fail on Hugs due to a bug on Hugs.
--, holds n $ \x y z -> none isInfinite [x,y,z] ==> okNum x y (z -: float)
--, holds n $ \x y z -> none isInfinite [x,y,z] ==> okNum x y (z -: double)
  , holds n $ okNum -:> rational
  , holds n $ okNumNonNegative -:> nat
  , holds n $ okNumNonNegative -:> natural
  , holds n $ (\x y -> x < y ==> x - y == 0) -:> nat
  , holds n $ (\x y -> x < y ==> x - y == 0) -:> natural

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

--none :: (a -> Bool) -> [a] -> Bool
--none p = not . or . map p

-- Copyright (c) 2015-2020 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import System.Exit (exitFailure)
-- import Test.LeanCheck -- already exported by Test
import Test.LeanCheck.Utils

import Data.List (elemIndices, sort, isPrefixOf)
import Data.Function (on)

main :: IO ()
main  =  do
  max <- getMaxTestsFromArgs 200
  case elemIndices False (tests max) of
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

  , holds n $ (*) `isDistributiveOver` (+) -:> int
  , fails n $ (+) `isDistributiveOver` (*) -:> int
  , holds n $ (*) `isLeftDistributiveOver` (+) -:> int
  , fails n $ (+) `isLeftDistributiveOver` (*) -:> int
  , holds n $ (*) `isRightDistributiveOver` (+) -:> int
  , fails n $ (+) `isRightDistributiveOver` (*) -:> int

  , holds n $ isSymmetric (==) -:> int
  , holds n $ isSymmetric (/=) -:> int
  , fails n $ isSymmetric (<=) -:> int

  , holds n $ isReflexive   (==) -:> int
  , holds n $ isIrreflexive (/=) -:> int

  , holds n $ (<)  `isFlipped` (>)  -:> int
  , holds n $ (<=) `isFlipped` (>=) -:> int
  , fails n $ (<)  `isFlipped` (>=) -:> int
  , fails n $ (<=) `isFlipped` (>)  -:> int

  , holds n $ isTransitive (==) -:> bool
  , holds n $ isTransitive (<)  -:> bool
  , holds n $ isTransitive (<=) -:> bool
  , fails n $ isTransitive (/=) -:> bool
  , holds n $ isTransitive (==) -:> int
  , holds n $ isTransitive (<)  -:> int
  , holds n $ isTransitive (<=) -:> int
  , fails n $ isTransitive (/=) -:> int

  , holds n $ isAsymmetric    (<)  -:> int
  , holds n $ isAntisymmetric (<=) -:> int
  , fails n $ isAsymmetric    (<=) -:> int
  , holds n $ isAsymmetric    (>)  -:> int
  , holds n $ isAntisymmetric (>=) -:> int
  , fails n $ isAsymmetric    (>=) -:> int

  , holds n $ isEquivalence (==) -:> int
  , holds n $ isEquivalence ((==) `on` fst) -:> (int,int)
  , holds n $ isEquivalence ((==) `on` length) -:> [int]

  , holds n $       isTotalOrder (<=) -:> int
  , holds n $ isStrictTotalOrder (<)  -:> int
  , fails n $       isTotalOrder (<)  -:> int
  , fails n $ isStrictTotalOrder (<=) -:> int
  , holds n $       isTotalOrder (>=) -:> int
  , holds n $ isStrictTotalOrder (>)  -:> int
  , fails n $       isTotalOrder (>)  -:> int
  , fails n $ isStrictTotalOrder (>=) -:> int

  , holds n $ isPartialOrder isPrefixOf -:> [int]
  , fails n $   isTotalOrder isPrefixOf -:> [int]

  , holds n $ isComparison compare -:> int
  , holds n $ isComparison compare -:> bool
  , holds n $ isComparison compare -:> ()

  , holds n $ okEqOrd -:> ()
  , holds n $ okEqOrd -:> int
  , holds n $ okEqOrd -:> char
  , holds n $ okEqOrd -:> bool
  , holds m $ okEqOrd -:> [()]
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

  , holds n $ isIdempotent id   -:> int
  , holds n $ isIdempotent abs  -:> int
  , holds n $ isIdempotent sort -:> [bool]
  , fails n $ isIdempotent not

  , holds n $ isIdentity id   -:> int
  , holds n $ isIdentity (+0) -:> int
  , holds m $ isIdentity sort -:> [()]
  , holds n $ isIdentity (not . not)
  , fails n $ isIdentity not

  , holds n $ isNeverIdentity not
  , fails n $ isNeverIdentity abs    -:> int
  , fails n $ isNeverIdentity negate -:> int
  ]
  where
  m  =  200

--none :: (a -> Bool) -> [a] -> Bool
--none p  =  not . or . map p

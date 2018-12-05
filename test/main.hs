-- Copyright (c) 2015-2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import System.Exit (exitFailure)
import Data.List (elemIndices)

import Test.LeanCheck
import Test.LeanCheck.Utils

import Data.Ratio
import Data.Int (Int, Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)

main :: IO ()
main =
  case elemIndices False tests of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure

tests :: [Bool]
tests =
  [ True

  -- interleave
  , [1,2,3] +| [0,0,0] == [1,0,2,0,3,0]
  , take 3 ([1,2] +| (0:undefined)) == [1,0,2]
  , [0,2..] +| [1,3..] =| 100 |= [0,1..]

  -- etc
  , tNatPairOrd 100
  , tNatTripleOrd 200
  , tNatQuadrupleOrd 300
  , tNatQuintupleOrd 400
  , tNatListOrd 500
  , tListsOfNatOrd 500
  , listsOf (tiers::[[Nat]]) =| 10 |= tiers

  -- tests!
  , counterExample 10 (\x y -> x + y /= (x::Int)) == Just ["0", "0"]
  , counterExample 10 (\x y -> x + y == (x::Int)) == Just ["0", "1"]
  , counterExample 10 (maybe True (==(0::Int))) == Just ["(Just 1)"]
  , holds 100 (\x -> x == (x::Int))

  -- For when NaN is in the enumeration (by default, it is not):
  --, fails 100 (\x -> x == (x::Float))  -- NaN != NaN  :-)
  --, counterExample 100 (\x -> x == (x::Float)) == Just ["NaN"]
  , counterExample 10 (\x y -> x + y == (x::Float))  == Just ["0.0","1.0"]
  , counterExample 10 (\x y -> x + y == (x::Double)) == Just ["0.0","1.0"]
  , holds          50 (\x -> x + 1 /= (x::Int))
  , counterExample 50 (\x -> x + 1 /= (x::Float))  == Just ["Infinity"]
    || counterExample 50 (\x -> x + 1 /= (x::Float)) == Just ["inf"] -- bug on Hugs 2006-09?
  , counterExample 50 (\x -> x + 1 /= (x::Double)) == Just ["Infinity"]
    || counterExample 50 (\x -> x + 1 /= (x::Float)) == Just ["inf"] -- bug on Hugs 2006-09?
  , allUnique (take 100 list :: [Float])
  , allUnique (take 500 list :: [Double])

  , allUnique (take 500 list :: [Rational])
  , allUnique (take 100 list :: [Ratio Nat])
  , orderedOn (\r -> numerator r + denominator r) (take 500 (list :: [Ratio Nat]))
  , orderedOn (\r -> abs (numerator r) + abs(denominator r)) (take 500 (list :: [Rational]))

  , list == [LT, EQ, GT]
  , orderedOn length (take 500 (list :: [[Ordering]]))
  , orderedOn length (take 500 (list :: [[Bool]]))

  , strictlyOrderedOn (\xs -> (sum $ map (+1) xs, xs)) (take 500 (list :: [[Word]]))

  , tPairEqParams 100
  , tTripleEqParams 100

  , tProductsIsFilterByLength (tiers :: [[ Nat ]])   10 `all` [1..10]
  , tProductsIsFilterByLength (tiers :: [[ Bool ]])   6 `all` [1..10]
  , tProductsIsFilterByLength (tiers :: [[ [Nat] ]])  6 `all` [1..10]

  , holds 100 $  (\/)  ==== zipWith' (++) [] [] -:> [[uint2]]
  , holds 100 $  (\/)  ==== zipWith' (++) [] [] -:> [[bool]]
  , holds 100 $ (\\//) ==== zipWith' (+|) [] [] -:> [[uint2]]
  , holds 100 $ (\\//) ==== zipWith' (+|) [] [] -:> [[bool]]

  , holds 100 $ \x -> x == (x :: Word)
  , holds 100 $ \x -> x == (x :: Word8)
  , holds 100 $ \x -> x == (x :: Word16)
  , holds 100 $ \x -> x == (x :: Word32)
  , holds 100 $ \x -> x == (x :: Word64)

  , holds 100 $ \x -> x == (x :: Int)
  , holds 100 $ \x -> x == (x :: Int8)
  , holds 100 $ \x -> x == (x :: Int16)
  , holds 100 $ \x -> x == (x :: Int32)
  , holds 100 $ \x -> x == (x :: Int64)
  ]

allUnique :: Ord a => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = x `notElem` xs
                && allUnique (filter (< x) xs)
                && allUnique (filter (> x) xs)


-- | 'zipwith\'' works similarly to 'zipWith', but takes neutral elements to
--   operate when one of the lists is exhausted, so, you don't loose elements.
--
-- > zipWith' f z e [x,y] [a,b,c,d] == [f x a, f y b, f z c, f z d]
--
-- > zipWith' f z e [x,y,z] [a] == [f x a, f y e, f z e]
--
-- > zipWith' (+) 0 0 [1,2,3] [1,2,3,4,5,6] == [2,4,6,4,5,6]
zipWith' :: (a->b->c) -> a -> b  -> [a] -> [b] -> [c]
zipWith' _ _  _  []     [] = []
zipWith' f _  zy xs     [] = map (`f` zy) xs
zipWith' f zx _  []     ys = map (f zx) ys
zipWith' f zx zy (x:xs) (y:ys) = f x y : zipWith' f zx zy xs ys

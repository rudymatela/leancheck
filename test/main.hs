-- Copyright (c) 2015-2020 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import System.Exit (exitFailure)
import Data.List (elemIndices)

-- import Test.LeanCheck -- already exported by Test
import Test.LeanCheck.Utils

import Data.Ratio
import Data.Complex
import Data.Int
import Data.Word

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

  -- interleave
  , [1,2,3] +| [0,0,0] == [1,0,2,0,3,0]
  , take 3 ([1,2] +| (0:undefined)) == [1,0,2]
  , [0,2..] +| [1,3..] =| n |= [0,1..]

  -- etc
  , tNatPairOrd n
  , tNatTripleOrd n
  , tNatQuadrupleOrd n
  , tNatQuintupleOrd n
  , tNatListOrd n
  , tListsOfNatOrd n
  , listsOf (tiers::[[Nat]]) =| 10 |= tiers

  -- tests!
  , counterExample n (\x y -> x + y /= (x::Int)) == Just ["0", "0"]
  , counterExample n (\x y -> x + y == (x::Int)) == Just ["0", "1"]
  , counterExample n (maybe True (==(0::Int))) == Just ["(Just 1)"]
  , holds n (\x -> x == (x::Int))

  -- For when NaN is in the enumeration (by default, it is not):
  --, fails 100 (\x -> x == (x::Float))  -- NaN != NaN  :-)
  --, counterExample 100 (\x -> x == (x::Float)) == Just ["NaN"]
  , counterExample n (\x y -> x + y == (x::Float))  == Just ["0.0","1.0"]
  , counterExample n (\x y -> x + y == (x::Double)) == Just ["0.0","1.0"]
  , holds          n (\x -> x + 1 /= (x::Int))
  , counterExample n (\x -> x + 1 /= (x::Float))  == Just ["Infinity"]
    || counterExample n (\x -> x + 1 /= (x::Float)) == Just ["inf"] -- bug on Hugs 2006-09?
  , counterExample n (\x -> x + 1 /= (x::Double)) == Just ["Infinity"]
    || counterExample n (\x -> x + 1 /= (x::Float)) == Just ["inf"] -- bug on Hugs 2006-09?
  , allUnique (take n list :: [Float])
  , allUnique (take n list :: [Double])

  , allUnique (take n list :: [Rational])
  , allUnique (take n list :: [Ratio Nat])

  , list == [LT, EQ, GT]
  , orderedOn length (take n (list :: [[Ordering]]))
  , orderedOn length (take n (list :: [[Bool]]))

  , strictlyOrderedOn (\xs -> (sum $ map (+1) xs, xs)) (take n (list :: [[Word]]))

  , tPairEqParams n
  , tTripleEqParams n

  , tProductsIsFilterByLength (tiers :: [[ Nat ]])   10 `all` [1..10]
  , tProductsIsFilterByLength (tiers :: [[ Bool ]])   6 `all` [1..10]
  , tProductsIsFilterByLength (tiers :: [[ [Nat] ]])  6 `all` [1..10]

  , holds n $  (\/)  ==== zipWith' (++) [] [] -:> [[uint2]]
  , holds n $  (\/)  ==== zipWith' (++) [] [] -:> [[bool]]
  , holds n $ (\\//) ==== zipWith' (+|) [] [] -:> [[uint2]]
  , holds n $ (\\//) ==== zipWith' (+|) [] [] -:> [[bool]]

  , holds n $ \x -> x == (x :: Word)
  , holds n $ \x -> x == (x :: Word8)
  , holds n $ \x -> x == (x :: Word16)
  , holds n $ \x -> x == (x :: Word32)
  , holds n $ \x -> x == (x :: Word64)

  , holds n $ \x -> x == (x :: Int)
  , holds n $ \x -> x == (x :: Int8)
  , holds n $ \x -> x == (x :: Int16)
  , holds n $ \x -> x == (x :: Int32)
  , holds n $ \x -> x == (x :: Int64)

  , holds n $ \x -> x == (x :: Complex Double)
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

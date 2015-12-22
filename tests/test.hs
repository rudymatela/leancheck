import System.Exit (exitFailure)
import Data.List (elemIndices)

import Test.Check
import Test.Check.Invariants
import Test.Types (Nat)

main :: IO ()
main =
  case elemIndices False tests of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure

tests =
  [ True

  -- interleave
  , [1,2,3] \/ [0,0,0] == [1,0,2,0,3,0]
  , take 3 ([1,2] \/ (0:undefined)) == [1,0,2]
  , let n = 100
        xs === ys = take n xs == take n ys
    in  ([0,2..] \/ [1,3..]) === [0,1..]

  -- etc
  , lsNatPairOrd 100
  , lsNatTripleOrd 200
  , lsNatQuadrupleOrd 300
  , lsNatQuintupleOrd 400
  , lsNatSixtupleOrd 500
  , lsNatListOrd 500
  , lsListsOfNatOrd 500

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
  , counterExample 50 (\x -> x + 1 /= (x::Double)) == Just ["Infinity"]
  , allUnique (take 100 $ list :: [Float])
  , allUnique (take 500 $ list :: [Double])

  , lsPairEqParams 100
  , lsTripleEqParams 100

  , lsProductsIsFilterByLength (listing :: [[ Nat ]])   10 `all` [1..10]
  , lsProductsIsFilterByLength (listing :: [[ Bool ]])   6 `all` [1..10]
  , lsProductsIsFilterByLength (listing :: [[ [Nat] ]])  6 `all` [1..10]
  ]

allUnique :: Ord a => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = x `notElem` xs
                && allUnique (lesser)
                && allUnique (greater)
  where lesser  = filter (< x) xs
        greater = filter (> x) xs

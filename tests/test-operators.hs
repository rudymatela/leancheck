import System.Exit (exitFailure)
import Data.List (elemIndices)
import Test.Check
import Test.Operators

import Data.List (sort)


argTypeOf :: (a -> b) -> a -> (a -> b)
argTypeOf = const

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
  , fails n $ abs === (* (-1))             `argTypeOf` (undefined :: Int)
  , holds n $ (+) ==== (\x y -> sum [x,y]) `argTypeOf` (undefined :: Int)
  , fails n $ (+) ==== (*)                 `argTypeOf` (undefined :: Int)

  , holds n $ const True  &&& const True  `argTypeOf` (undefined :: Bool)
  , fails n $ const False &&& const True  `argTypeOf` (undefined :: Int)
  , holds n $ const False ||| const True  `argTypeOf` (undefined :: Int)
  , fails n $ const False ||| const False `argTypeOf` (undefined :: Int)

  , holds n $ commutative (+)  `argTypeOf` (undefined :: Int)
  , holds n $ commutative (*)  `argTypeOf` (undefined :: Int)
  , holds n $ commutative (++) `argTypeOf` (undefined :: [()])
  , holds n $ commutative (&&)
  , holds n $ commutative (||)
  , fails n $ commutative (-)  `argTypeOf` (undefined :: Int)
  , fails n $ commutative (++) `argTypeOf` (undefined :: [Bool])
  , fails n $ commutative (==>)

  , holds n $ associative (+)  `argTypeOf` (undefined :: Int)
  , holds n $ associative (*)  `argTypeOf` (undefined :: Int)
  , holds n $ associative (++) `argTypeOf` (undefined :: [Int])
  , holds n $ associative (&&)
  , holds n $ associative (||)
  , fails n $ associative (-)  `argTypeOf` (undefined :: Int)
  , fails n $ associative (==>)

  , holds n $ distributive (*) (+) `argTypeOf` (undefined :: Int)
  , fails n $ distributive (+) (*) `argTypeOf` (undefined :: Int)

  , holds n $ transitive (==) `argTypeOf` (undefined :: Bool)
  , holds n $ transitive (<)  `argTypeOf` (undefined :: Bool)
  , holds n $ transitive (<=) `argTypeOf` (undefined :: Bool)
  , fails n $ transitive (/=) `argTypeOf` (undefined :: Bool)
  , holds n $ transitive (==) `argTypeOf` (undefined :: Int)
  , holds n $ transitive (<)  `argTypeOf` (undefined :: Int)
  , holds n $ transitive (<=) `argTypeOf` (undefined :: Int)
  , fails n $ transitive (/=) `argTypeOf` (undefined :: Int)

  , holds n $ idempotent id   `argTypeOf` (undefined :: Int)
  , holds n $ idempotent abs  `argTypeOf` (undefined :: Int)
  , holds n $ idempotent sort `argTypeOf` (undefined :: [Bool])
  , fails n $ idempotent not

  , holds n $ identity id   `argTypeOf` (undefined :: Int)
  , holds n $ identity (+0) `argTypeOf` (undefined :: Int)
  , holds n $ identity sort `argTypeOf` (undefined :: [()])
  , holds n $ identity (not . not)
  , fails n $ identity not

  , holds n $ notIdentity not
  , fails n $ notIdentity abs    `argTypeOf` (undefined :: Int)
  , fails n $ notIdentity negate `argTypeOf` (undefined :: Int)
  ]


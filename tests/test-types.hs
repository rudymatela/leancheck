import System.Exit (exitFailure)
import Data.List (elemIndices,delete)
import Test.Types
import Test.Check (list,fails)

main :: IO ()
main =
  case elemIndices False tests of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure


-- | Given the number of bits, generates a range for signed/unsigned integers
--   of that width.
signedRange,unsignedRange :: (Num n) => Int -> [n]
signedRange n   = map fromIntegral [-(2^(n-1))..(2^(n-1))-1]
unsignedRange n = map fromIntegral [0..(2^n)-1]


tests =
  [ True


  , list `permutation` [minBound..maxBound :: Int1]
  , list `permutation` [minBound..maxBound :: Int2]
  , list `permutation` [minBound..maxBound :: Int3]
  , list `permutation` [minBound..maxBound :: Int4]

  , list `permutation` [minBound..maxBound :: UInt1]
  , list `permutation` [minBound..maxBound :: UInt2]
  , list `permutation` [minBound..maxBound :: UInt3]
  , list `permutation` [minBound..maxBound :: UInt4]


  , [minBound..maxBound :: Int1] == signedRange 1
  , [minBound..maxBound :: Int2] == signedRange 2
  , [minBound..maxBound :: Int3] == signedRange 3
  , [minBound..maxBound :: Int4] == signedRange 4

  , [minBound..maxBound :: UInt1] == unsignedRange 1
  , [minBound..maxBound :: UInt2] == unsignedRange 2
  , [minBound..maxBound :: UInt3] == unsignedRange 3
  , [minBound..maxBound :: UInt4] == unsignedRange 4


  -- abs minBound == minBound
  , fails 100 (\i -> abs i > (0::Int1))
  , fails 100 (\i -> abs i > (0::Int2))
  , fails 100 (\i -> abs i > (0::Int3))
  , fails 100 (\i -> abs i > (0::Int4))

  -- maxBound + 1 == minBound
  , fails 100 (\i -> i + 1 < (i::Int1))
  , fails 100 (\i -> i + 1 < (i::Int2))
  , fails 100 (\i -> i + 1 < (i::Int3))
  , fails 100 (\i -> i + 1 < (i::Int4))
  ]


permutation :: Eq a => [a] -> [a] -> Bool
[]     `permutation` []    = True
(_:_)  `permutation` []    = False
[]     `permutation` (_:_) = False
(x:xs) `permutation` ys    = x `elem` ys  &&  xs `permutation` delete x ys

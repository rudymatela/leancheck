{-# LANGUAGE CPP #-}
-- Copyright (c) 2015-2020 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test
import System.Exit (exitFailure)
import Data.List (elemIndices,delete,isPrefixOf)
import Test.LeanCheck.Utils.Types
import Data.Word
import Data.Int
import Data.Char hiding (Space)

main :: IO ()
main  =  do
  max <- getMaxTestsFromArgs 200
  case elemIndices False (tests max) of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure


-- | Given the number of bits, generates a range for signed/unsigned integers
--   of that width.
signedRange,unsignedRange :: (Num n) => Int -> [n]
signedRange n   = map fromIntegral [-(2^(n-1))..(2^(n-1))-1]
unsignedRange n = map fromIntegral [0..(2^n)-1]

tests :: Int -> [Bool]
tests n =
  [ True

  , list `permutation` [minBound..maxBound :: Int1]
  , list `permutation` [minBound..maxBound :: Int2]
  , list `permutation` [minBound..maxBound :: Int3]
  , list `permutation` [minBound..maxBound :: Int4]

  , list `permutation` [minBound..maxBound :: UInt1]
  , list `permutation` [minBound..maxBound :: UInt2]
  , list `permutation` [minBound..maxBound :: UInt3]
  , list `permutation` [minBound..maxBound :: UInt4]

  , list `permutation` [minBound..maxBound :: Nat1]
  , list `permutation` [minBound..maxBound :: Nat2]
  , list `permutation` [minBound..maxBound :: Nat3]
  , list `permutation` [minBound..maxBound :: Nat4]
  , list `permutation` [minBound..maxBound :: Nat5]
  , list `permutation` [minBound..maxBound :: Nat6]
  , list `permutation` [minBound..maxBound :: Nat7]

  , list `permutation` [minBound..maxBound :: A]
  , list `permutation` [minBound..maxBound :: B]
  , list `permutation` [minBound..maxBound :: C]
  , list `permutation` [minBound..maxBound :: D]
  , list `permutation` [minBound..maxBound :: E]
  , list `permutation` [minBound..maxBound :: F]

  , tiers == [[0], [1], [2], [3], [4], [5 :: A]]
  , tiers == [[0], [1], [2], [3], [4], [5 :: B]]
  , tiers == [[0], [1], [2], [3], [4], [5 :: C]]
  , tiers == [[0], [1], [2], [3], [4], [5 :: D]]
  , tiers == [[0], [1], [2], [3], [4], [5 :: E]]
  , tiers == [[0], [1], [2], [3], [4], [5 :: F]]

  , map unX list `permutation` [minBound..maxBound :: Int8]
  , map unX list `permutation` [minBound..maxBound :: Int4]
  , map unX list `permutation` [minBound..maxBound :: Int3]
  , map unX list `permutation` [minBound..maxBound :: Int2]
  , map unX list `permutation` [minBound..maxBound :: Int1]
  , map unX list `permutation` [minBound..maxBound :: Word8]
  , map unX list `permutation` [minBound..maxBound :: Word4]
  , map unX list `permutation` [minBound..maxBound :: Word3]
  , map unX list `permutation` [minBound..maxBound :: Word2]
  , map unX list `permutation` [minBound..maxBound :: Word1]
  , map unX list `permutation` [minBound..maxBound :: Nat7]
  , map unX list `permutation` [minBound..maxBound :: Nat6]
  , map unX list `permutation` [minBound..maxBound :: Nat5]
  , map unX list `permutation` [minBound..maxBound :: Nat4]
  , map unX list `permutation` [minBound..maxBound :: Nat3]
  , map unX list `permutation` [minBound..maxBound :: Nat2]
  , map unX list `permutation` [minBound..maxBound :: Nat1]

  , (prefiX :: [Int8])  `isPrefixOf` map unX list
  , (prefiX :: [Int16]) `isPrefixOf` map unX list
  , (prefiX :: [Int32]) `isPrefixOf` map unX list
  , (prefiX :: [Int])   `isPrefixOf` map unX list

  , (prefiXN :: [Word8])  `isPrefixOf` map unX list
  , (prefiXN :: [Word16]) `isPrefixOf` map unX list
  , (prefiXN :: [Word32]) `isPrefixOf` map unX list
  , (prefiXN :: [Word])   `isPrefixOf` map unX list

  , (prefiXs :: [[Int8]])  `isPrefixOf` map unXs list
  , (prefiXs :: [[Int16]]) `isPrefixOf` map unXs list
  , (prefiXs :: [[Int32]]) `isPrefixOf` map unXs list
  , (prefiXs :: [[Int]])   `isPrefixOf` map unXs list

  , (prefiXNs :: [[Word8]])  `isPrefixOf` map unXs list
  , (prefiXNs :: [[Word16]]) `isPrefixOf` map unXs list
  , (prefiXNs :: [[Word32]]) `isPrefixOf` map unXs list
  , (prefiXNs :: [[Word]])   `isPrefixOf` map unXs list

#ifndef __HUGS__
  -- Hugs returns an arithmetic overflow error for these tests
  , (prefiX   :: [Int64])    `isPrefixOf` map unX  list
  , (prefiXN  :: [Word64])   `isPrefixOf` map unX  list
  , (prefiXs  :: [[Int64]])  `isPrefixOf` map unXs list
  , (prefiXNs :: [[Word64]]) `isPrefixOf` map unXs list
#endif

  , [minBound..maxBound :: Int1] == signedRange 1
  , [minBound..maxBound :: Int2] == signedRange 2
  , [minBound..maxBound :: Int3] == signedRange 3
  , [minBound..maxBound :: Int4] == signedRange 4

  , [minBound..maxBound :: UInt1] == unsignedRange 1
  , [minBound..maxBound :: UInt2] == unsignedRange 2
  , [minBound..maxBound :: UInt3] == unsignedRange 3
  , [minBound..maxBound :: UInt4] == unsignedRange 4


  -- abs minBound == minBound
  , fails n (\i -> abs i > (0::Int1))
  , fails n (\i -> abs i > (0::Int2))
  , fails n (\i -> abs i > (0::Int3))
  , fails n (\i -> abs i > (0::Int4))

  -- maxBound + 1 == minBound
  , fails n (\i -> i + 1 < (i::Int1))
  , fails n (\i -> i + 1 < (i::Int2))
  , fails n (\i -> i + 1 < (i::Int3))
  , fails n (\i -> i + 1 < (i::Int4))

  , holds n (\(Nat n) -> n >= 0)
  , holds n (\(Natural n) -> n >= 0)

  , holds n $ \(Space c) -> isSpace c
  , holds n $ \(Lower c) -> isLower c
  , holds n $ \(Upper c) -> isUpper c
  , holds n $ \(Alpha c) -> isAlpha c
  , holds n $ \(Digit c) -> isDigit c
  , holds n $ \(AlphaNum c) -> isAlphaNum c
  , holds n $ \(Letter c) -> isLetter c

  , holds n $ \(Spaces s) -> all isSpace s
  , holds n $ \(Lowers s) -> all isLower s
  , holds n $ \(Uppers s) -> all isUpper s
  , holds n $ \(Alphas s) -> all isAlpha s
  , holds n $ \(Digits s) -> all isDigit s
  , holds n $ \(AlphaNums s) -> all isAlphaNum s
  , holds n $ \(Letters s)   -> all isLetter s
  ]
  where
  unXs (Xs xs) = xs


prefiX :: (Bounded a, Integral a) => [a]
prefiX =
  [ 0
  , 1, -1
  , maxBound, minBound
  , 2, -2
  , maxBound-1, minBound+1
  , 3, -3
  , maxBound-2, minBound+2
  ]

prefiXs :: (Bounded a, Integral a) => [[a]]
prefiXs =
  [ []
  , [0]
  , [0,0], [1]
  , [0,0,0], [0,1], [1,0], [-1]
  , [0,0,0,0], [0,0,1], [0,1,0], [0,-1], [1,0,0], [1,1], [-1,0], [maxBound]
  , [0,0,0,0,0], [0,0,0,1], [0,0,1,0], [0,0,-1]
    , [0,1,0,0], [0,1,1], [0,-1,0], [0,maxBound]
    , [1,0,0,0], [1,0,1], [1,1,0], [1,-1]
    , [-1,0,0], [-1,1], [maxBound,0], [minBound]
  ]

prefiXN :: (Bounded a, Integral a) => [a]
prefiXN =
  [ 0
  , 1, 2
  , maxBound
  , 3, 4
  , maxBound-1
  , 5, 6
  , maxBound-2
  ]

prefiXNs :: (Bounded a, Integral a) => [[a]]
prefiXNs =
  [ []
  , [0]
  , [0,0], [1]
  , [0,0,0], [0,1], [1,0], [2]
  , [0,0,0,0], [0,0,1], [0,1,0], [0,2], [1,0,0], [1,1], [2,0], [maxBound]
  ]

permutation :: Eq a => [a] -> [a] -> Bool
[]     `permutation` []    = True
(_:_)  `permutation` []    = False
[]     `permutation` (_:_) = False
(x:xs) `permutation` ys    = x `elem` ys  &&  xs `permutation` delete x ys

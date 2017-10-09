-- Copyright (c) 2015-2017 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE TemplateHaskell, CPP #-}
import Test
import Test.LeanCheck
import Test.LeanCheck.Derive
import System.Exit (exitFailure)
import Data.List (elemIndices,sort)
import Test.LeanCheck.Utils.Operators

data D0       = D0                    deriving Show
data D1 a     = D1 a                  deriving Show
data D2 a b   = D2 a b                deriving Show
data D3 a b c = D3 a b c              deriving Show
data C1 a     =           C11 a | C10 deriving Show
data C2 a b   = C22 a b | C21 a | C20 deriving Show
data I a b    = a :+ b                deriving Show

deriveListable ''D0
deriveListable ''D1
deriveListable ''D2
deriveListable ''D3
deriveListable ''C1
deriveListable ''C2
deriveListable ''I

-- Nested datatype cascade
data Nested = Nested N0 (N1 Int) (N2 Int Int)
data N0     = R0 Int
data N1 a   = R1 a
data N2 a b = R2 a b
deriveListableCascading ''Nested

-- Recursive nested datatype cascade
data RN      = RN RN0 (RN1 Int) (RN2 Int RN)
data RN0     = Nest0 Int | Recurse0 RN
data RN1 a   = Nest1 a   | Recurse1 RN
data RN2 a b = Nest2 a b | Recurse2 RN
deriveListableCascading ''RN

-- Type synonyms
data Pair a = Pair a a
type Alias a = Pair a
-- deriveListable ''Alias -- this will fail
deriveListableCascading ''Alias
deriveListableIfNeeded ''Alias -- only works because instance already exists

-- Nested type synonyms
data Triple a = Triple a a a
type Tralias a = Triple a
data Pairiple a = Pairriple (Tralias a) (Tralias a)
deriveListableCascading ''Pairiple

-- Those should have no effect (instance already exists):
{- uncommenting those should generate warnings
deriveListable ''Bool
deriveListable ''Maybe
deriveListable ''Either
-}

-- Those should not generate warnings
deriveListableIfNeeded ''Bool
deriveListableIfNeeded ''Maybe
deriveListableIfNeeded ''Either

main :: IO ()
main =
  case elemIndices False (tests 100) of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure

tests n =
  [ True

  , map unD0 list =| n |= list
  , map unD1 list =| n |= (list :: [Int])
  , map unD2 list =| n |= (list :: [(Int,Int)])
  , map unD3 list =| n |= (list :: [(Int,Int,Int)])

  , map unD1 list == (list :: [()])
  , map unD2 list == (list :: [((),())])
  , map unD3 list == (list :: [((),(),())])

  , map unD1 list == (list :: [Bool])
  , map unD2 list == (list :: [(Bool,Bool)])
  , map unD3 list == (list :: [(Bool,Bool,Bool)])

  , (tiers :: [[ Bool       ]]) =| 6 |= $(deriveTiers ''Bool)
  , (tiers :: [[ [Int]      ]]) =| 6 |= $(deriveTiers ''[])
  , (tiers :: [[ [Bool]     ]]) =| 6 |= $(deriveTiers ''[])
  , (tiers :: [[ Maybe Int  ]]) =| 6 |= $(deriveTiers ''Maybe)
  , (tiers :: [[ Maybe Bool ]]) =| 6 |= $(deriveTiers ''Maybe)
  , ([]:tiers :: [[Either Bool Int]]) =$ map sort . take 6 $= $(deriveTiers ''Either)
  ]
  where
  unD0 (D0)       = ()
  unD1 (D1 x)     = (x)
  unD2 (D2 x y)   = (x,y)
  unD3 (D3 x y z) = (x,y,z)

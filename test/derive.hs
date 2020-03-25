-- Copyright (c) 2015-2020 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE TemplateHaskell, CPP #-}
import Test
-- import Test.LeanCheck -- already exported by Test
import Test.LeanCheck.Derive
import System.Exit (exitFailure)
import Data.List (elemIndices,sort)
import Test.LeanCheck.Utils

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

-- recursive datatypes
data Peano = Zero | Succ Peano deriving Show
data List a = a :- List a | Nil deriving Show
data Bush a = Bush a :-: Bush a | Leaf a deriving (Show, Eq)
data Tree a = Node (Tree a) a (Tree a) | Null deriving (Show, Eq)

deriveListable ''Peano
deriveListable ''List
deriveListable ''Bush
deriveListable ''Tree

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
main  =  do
  max <- getMaxTestsFromArgs 200
  case elemIndices False (tests max) of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure

tests :: Int -> [Bool]
tests n  =
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

  , map peanoToNat list =| n |= list
  , map listToList list =| n |= (list :: [[Bool]])
  , map listToList list =| n |= (list :: [[Int]])

  , mapT peanoToNat tiers =| 6 |= tiers
  , mapT listToList tiers =| 6 |= (tiers :: [[ [Bool] ]])
  , mapT listToList tiers =| 6 |= (tiers :: [[ [Int] ]])

  , take 6 (list :: [Bush Bool])
    == [ Leaf False
       , Leaf True
       , Leaf False :-: Leaf False
       , Leaf False :-: Leaf True
       , Leaf True :-: Leaf False
       , Leaf True :-: Leaf True
       ]

  , take 6 (list :: [Tree Bool])
    == [ Null
       , Node Null False Null
       , Node Null True Null
       , Node Null False (Node Null False Null)
       , Node Null False (Node Null True Null)
       , Node Null True (Node Null False Null)
       ]

  , (tiers :: [[ Bool       ]]) =| 6 |= $(deriveTiers ''Bool)
  , (tiers :: [[ [Int]      ]]) =| 6 |= $(deriveTiers ''[])
  , (tiers :: [[ [Bool]     ]]) =| 6 |= $(deriveTiers ''[])
  , (tiers :: [[ Maybe Int  ]]) =| 6 |= $(deriveTiers ''Maybe)
  , (tiers :: [[ Maybe Bool ]]) =| 6 |= $(deriveTiers ''Maybe)
  , ([]:tiers :: [[Either Bool Int]]) =$ map sort . take 6 $= $(deriveTiers ''Either)

  , (list :: [ Bool       ]) =| n |= $(deriveList ''Bool)
  , (list :: [ [Int]      ]) =| n |= $(deriveList ''[])
  , (list :: [ [Bool]     ]) =| n |= $(deriveList ''[])
  , (list :: [ Maybe Int  ]) =| n |= $(deriveList ''Maybe)
  , (list :: [ Maybe Bool ]) =| n |= $(deriveList ''Maybe)
  ]
  where
  unD0 (D0)       = ()
  unD1 (D1 x)     = (x)
  unD2 (D2 x y)   = (x,y)
  unD3 (D3 x y z) = (x,y,z)

peanoToNat :: Peano -> Nat
peanoToNat Zero = 0
peanoToNat (Succ n) = 1 + peanoToNat n

listToList :: List a -> [a]
listToList Nil = []
listToList (x :- xs) = x : listToList xs

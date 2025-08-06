-- Copyright (c) 2015-2024 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE TemplateHaskell, CPP #-}
import Test
-- import Test.LeanCheck -- already exported by Test
import Test.LeanCheck.Derive
import Test.LeanCheck.Utils
import Data.List (sort)

data D0        =  D0                     deriving Show
newtype D1 a   =  D1 a                   deriving Show
data D2 a b    =  D2 a b                 deriving Show
data D3 a b c  =  D3 a b c               deriving Show
data C1 a      =            C11 a | C10  deriving Show
data C2 a b    =  C22 a b | C21 a | C20  deriving Show
data I a b     =  a :+ b                 deriving Show

deriveListable ''D0
deriveListable ''D1
deriveListable ''D2
deriveListable ''D3
deriveListable ''C1
deriveListable ''C2
deriveListable ''I

-- recursive datatypes
data Peano  =  Zero | Succ Peano  deriving Show
data Lst a  =  a :- Lst a | Nil  deriving Show
data Bush a  =  Bush a :-: Bush a | Leaf a  deriving (Show, Eq)
data Tree a  =  Node (Tree a) a (Tree a) | Null  deriving (Show, Eq)

deriveListable ''Peano
deriveListable ''Lst
deriveListable ''Bush
deriveListable ''Tree

-- Nested datatype cascade
data Nested  =  Nested N0 (N1 Int) (N2 Int Int)
newtype N0   =  R0 Int
newtype N1 a =  R1 a
data N2 a b  =  R2 a b
deriveListableCascading ''Nested

-- Recursive nested datatype cascade
data RN       =  RN RN0 (RN1 Int) (RN2 Int RN)
data RN0      =  Nest0 Int | Recurse0 RN
data RN1 a    =  Nest1 a   | Recurse1 RN
data RN2 a b  =  Nest2 a b | Recurse2 RN
deriveListableCascading ''RN

-- Type synonyms
data Pair a  =  Pair a a
type Alias a  =  Pair a
-- deriveListable ''Alias -- this will fail
deriveListableCascading ''Alias
deriveListableIfNeeded ''Alias -- only works because instance already exists

-- Nested type synonyms
data Triple a  =  Triple a a a
type Tralias a  =  Triple a
data Pairiple a  =  Pairriple (Tralias a) (Tralias a)
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
main  =  mainTest tests 200

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
  , map listLst list =| n |= (list :: [[Bool]])
  , map listLst list =| n |= (list :: [[Int]])

  , mapT peanoToNat tiers =| 6 |= tiers
  , mapT listLst tiers =| 6 |= (tiers :: [[ [Bool] ]])
  , mapT listLst tiers =| 6 |= (tiers :: [[ [Int] ]])

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
  , (tiers :: [[Either Bool Int]])
    =$ map sort . take 6 $= $(deriveTiers ''Either)

  , (list :: [ Bool       ]) =| n |= $(deriveList ''Bool)
  , (list :: [ [Int]      ]) =| n |= $(deriveList ''[])
  , (list :: [ [Bool]     ]) =| n |= $(deriveList ''[])
  , (list :: [ Maybe Int  ]) =| n |= $(deriveList ''Maybe)
  , (list :: [ Maybe Bool ]) =| n |= $(deriveList ''Maybe)

  , map length (take 6 $ tiers :: [[D0]]) == [1]

  , map length (take 6 $ tiers :: [[D1 Int]]) == [1,1,1,1,1,1]
  , map length (take 6 $ tiers :: [[D2 Int Int]]) == [1,2,3,4,5,6]
  , map length (take 6 $ tiers :: [[D3 Int Int Int]]) == [1,3,6,10,15,21]
  , map length (take 6 $ tiers :: [[D1 Bool]]) == [2]
  , map length (take 6 $ tiers :: [[D2 Bool Bool]]) == [4]

  , map length (take 6 $ tiers :: [[C1 Int]]) == [1,1,1,1,1,1]
  , map length (take 6 $ tiers :: [[C2 Int Int]]) == [1,2,3,4,5,6]

  , map length (take 6 $ tiers :: [[ [D1 Int] ]]) == [1,1,2,4,8,16]
  , map length (take 6 $ tiers :: [[ [D1 Bool] ]]) == [1,2,4,8,16,32]
  , map length (take 6 $ tiers :: [[ [D2 Int Int] ]]) == [1,1,3,8,21,55]
  , map length (take 6 $ tiers :: [[ [D2 Int Bool] ]]) == [1,2,6,18,54,162]

  , map length (take 6 $ tiers :: [[ Lst Int ]]) == [1,1,2,4,8,16]
  , map length (take 6 $ tiers :: [[ Lst Bool ]]) == [1,2,4,8,16,32]
  ]
  where
  unD0 (D0)        =  ()
  unD1 (D1 x)      =  (x)
  unD2 (D2 x y)    =  (x,y)
  unD3 (D3 x y z)  =  (x,y,z)

peanoToNat :: Peano -> Nat
peanoToNat Zero  =  0
peanoToNat (Succ n)  =  1 + peanoToNat n

listLst :: Lst a -> [a]
listLst Nil  =  []
listLst (x :- xs)  =  x : listLst xs
